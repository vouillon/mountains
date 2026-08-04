(* The arithmetic core of [Hd_dem.blend], split out of it so that there is one
   explicit boundary -- plain arrays and scalars, no Brr, no Lwt, no fetching --
   that a hand-written wasm implementation can reproduce and be differentially
   tested against (see PLAN.md, "blend.wat in the worker").

   Deliberately kept in the shape it had inside [Hd_dem]: every floating-point
   expression is written exactly as it was, because the grid this produces is
   compared bit for bit both against the implementation it replaces and against
   the wasm one. That is also why the source is passed as an extracted *window*
   rather than the whole tile -- it is the window that a worker would receive,
   so the reference and the wasm version index the same bytes. *)

open Bigarray

(* Anything below this counts as "no data here", which covers two distinct
   things: IGN answers -9999 for ground outside its coverage, and [Hd_dem]
   pre-fills a block with -99999 so that a request which never arrives reads the
   same way. Both are faded back into the surface beneath. *)
let nodata_limit = -500.

let smoothstep t =
  if t <= 0. then 0. else if t >= 1. then 1. else t *. t *. (3. -. (2. *. t))

(* Everything the blend needs to know about the two grids involved. Rows count
   from the south on both sides, matching [Dem_loader]; the refinement's own
   samples arrive north-up, which the resampling loop accounts for. *)
type params = {
  size : int; (* samples per side of the refinement *)
  px_arcsec : float; (* its sample spacing *)
  raw_origin_x : float; (* arcseconds from the anchor to its column 0 *)
  raw_origin_y : float; (* ... and to its southernmost row *)
  src_size : int; (* samples per side of the surface beneath *)
  src_origin_x : float;
  src_origin_y : float;
  src_px_arcsec : float;
  src_height_scale : float; (* metres per u16 step of the source *)
  src_height_offset : float; (* metres at source u16 zero *)
  fade_x : float; (* fade width, in refinement samples, per axis *)
  fade_y : float;
}

(* Fine sample [j] sits [o + j * px_arcsec] arcseconds from the anchor; the
   source's own origin and spacing turn that into a fractional source index,
   clamped so the bilinear pair [i], [i + 1] stays inside the grid. The
   refinement's samples fall between the source's at a fixed fractional step, so
   neither grid need be a power-of-two multiple of the other.

   Written so that the base case is bit-identical to the hardcoded form this
   replaced: subtracting a negative origin is exactly adding, addition is
   commutative in IEEE754, the grouping is unchanged, and dividing by a spacing
   of 1.0 is exact. *)
let source_index p origin o j =
  Float.max 0.
    (Float.min
       (float (p.src_size - 2))
       ((o -. origin +. (float j *. p.px_arcsec)) /. p.src_px_arcsec))

(* The rectangle of the source that a blend reads: the bilinear window, plus the
   one row and column beyond it that the range scan reaches. This is what has to
   be copied to a worker -- ~636^2 samples for [l13] over the base, so 0.8 MB
   rather than the 32 MB of the whole tile. *)
type geometry = {
  col_lo : int;
  row_lo : int;
  n_cols : int; (* columns spanned by the bilinear window *)
  n_rows : int;
  win_cols : int; (* columns to copy, from [col_lo]; >= [n_cols] *)
  win_rows : int;
}

let geometry p =
  let last = p.src_size - 1 in
  let col_of j = source_index p p.src_origin_x p.raw_origin_x j in
  let col_lo = int_of_float (floor (col_of 0)) in
  let col_hi = int_of_float (floor (col_of (p.size - 1))) + 1 in
  let row_of u = source_index p p.src_origin_y p.raw_origin_y u in
  let row_lo = int_of_float (floor (row_of 0)) in
  let row_hi = int_of_float (floor (row_of (p.size - 1))) + 1 in
  {
    col_lo;
    row_lo;
    n_cols = col_hi - col_lo + 1;
    n_rows = row_hi - row_lo + 1;
    win_cols = min last (col_hi + 1) - col_lo + 1;
    win_rows = min last (row_hi + 1) - row_lo + 1;
  }

(* Chessboard distance in texels to the nearest nodata sample, saturated at
   255: two sweeps over the raster. Only built when the patch actually holds
   nodata, which means a location near the edge of French coverage. *)
let nodata_distance ~size (src : (float, float32_elt, c_layout) Array1.t) =
  (* [Bytes] rather than a Bigarray: this is touched some fifty million times
     over the two sweeps, and Bigarray element access does not compile to a plain
     load here -- swapping the scratch alone took the transform from 880 ms to a
     fraction of it on the 2048 block. *)
  let d = Bytes.make (size * size) '\000' in
  for i = 0 to size - 1 do
    let row = i * size in
    for j = 0 to size - 1 do
      let v =
        if Array1.unsafe_get src (row + j) < nodata_limit then 0
        else begin
          (* Shadowed lets rather than a [ref] and a [consider] closure: this
             runs twice over every sample of the block -- four million for
             [l13] -- and the per-sample allocation dominated the whole blend. *)
          let m = 255 in
          let m =
            if i > 0 then
              min m (Char.code (Bytes.unsafe_get d (row - size + j)))
            else m
          in
          let m =
            if i > 0 && j > 0 then
              min m (Char.code (Bytes.unsafe_get d (row - size + j - 1)))
            else m
          in
          let m =
            if i > 0 && j < size - 1 then
              min m (Char.code (Bytes.unsafe_get d (row - size + j + 1)))
            else m
          in
          let m =
            if j > 0 then min m (Char.code (Bytes.unsafe_get d (row + j - 1)))
            else m
          in
          if m >= 255 then 255 else m + 1
        end
      in
      Bytes.unsafe_set d (row + j) (Char.unsafe_chr v)
    done
  done;
  for i = size - 1 downto 0 do
    let row = i * size in
    for j = size - 1 downto 0 do
      let m0 = Char.code (Bytes.unsafe_get d (row + j)) in
      if m0 > 0 then begin
        let m = m0 in
        let m =
          if i < size - 1 then
            min m (Char.code (Bytes.unsafe_get d (row + size + j)) + 1)
          else m
        in
        let m =
          if i < size - 1 && j > 0 then
            min m (Char.code (Bytes.unsafe_get d (row + size + j - 1)) + 1)
          else m
        in
        let m =
          if i < size - 1 && j < size - 1 then
            min m (Char.code (Bytes.unsafe_get d (row + size + j + 1)) + 1)
          else m
        in
        let m =
          if j < size - 1 then
            min m (Char.code (Bytes.unsafe_get d (row + j + 1)) + 1)
          else m
        in
        Bytes.unsafe_set d (row + j) (Char.unsafe_chr m)
      end
    done
  done;
  d

type result = {
  data : (int, int8_unsigned_elt, c_layout) Array1.t;
      (* [size] rows of [size] little-endian u16, row 0 southernmost *)
  height_scale : float;
  height_offset : float;
  range : float; (* metres spanned by the bound, for logging *)
}

let run p ~(samples : (float, float32_elt, c_layout) Array1.t)
    ~(win : (int, int8_unsigned_elt, c_layout) Array1.t) =
  let size = p.size in
  let src = samples in
  let has_nodata = ref false and has_data = ref false in
  let lo = ref infinity and hi = ref neg_infinity in
  for i = 0 to (size * size) - 1 do
    let h = Array1.unsafe_get src i in
    if h < nodata_limit then has_nodata := true
    else begin
      has_data := true;
      if h < !lo then lo := h;
      if h > !hi then hi := h
    end
  done;
  if not !has_data then None
  else begin
    let g = geometry p in
    let dist = if !has_nodata then Some (nodata_distance ~size src) else None in
    let fade_nodata = Float.min p.fade_x p.fade_y in
    (* In metres, decoded with the source's own quantisation: the surface beneath
       may itself be a refinement with a scale of its own. Indices are absolute
       source ones; clamping them into the window reproduces the clamp to the
       tile's last row and column that the unwindowed form applied, because the
       window ends exactly there whenever it is the tile that runs out. *)
    let get_base row col =
      let r = min (g.win_rows - 1) (row - g.row_lo) in
      let c = min (g.win_cols - 1) (col - g.col_lo) in
      let o = ((r * g.win_cols) + c) * 2 in
      let low = Array1.unsafe_get win o in
      let high = Array1.unsafe_get win (o + 1) in
      (float_of_int ((high lsl 8) lor low) *. p.src_height_scale)
      +. p.src_height_offset
    in
    let col_of j = source_index p p.src_origin_x p.raw_origin_x j in
    let bx = Array.make size 0 and fx = Array.make size 0. in
    for j = 0 to size - 1 do
      let c = col_of j in
      let b = floor c in
      bx.(j) <- int_of_float b - g.col_lo;
      fx.(j) <- c -. b
    done;
    (* Range of the output, bounded rather than measured. Every blended value is
       [b + f * (h - b)] with [f] in [0, 1], so it lies on the segment between the
       surface beneath and the refinement, hence inside the union of their ranges.
       Using the bound lets the loop quantise as it goes: measuring the exact
       range needed a float buffer of every sample (16 MB at 2048) and a second
       pass over it, which cost more than the resampling itself -- Bigarray access
       is not compiled to a plain load by wasm_of_ocaml today. The bound is wider
       than the truth, so the step is coarser -- 7.98 cm against the 6.06 cm an
       exact range gives for [l13] at Mont Blanc, still well inside the 14.5 cm
       the base scale imposed. *)
    for j = 0 to g.n_rows do
      for i = 0 to g.n_cols do
        let b = get_base (g.row_lo + j) (g.col_lo + i) in
        if b < !lo then lo := b;
        if b > !hi then hi := b
      done
    done;
    let height_offset = !lo in
    let height_scale = Float.max 1e-6 ((!hi -. !lo) /. 65535.) in
    let inv = 1. /. height_scale in
    let out = Array1.create int8_unsigned c_layout (size * size * 2) in
    (* One base row pair resampled per base row rather than per sample. *)
    let rowa = Array.make (g.n_cols + 1) 0. in
    let rowb = Array.make (g.n_cols + 1) 0. in
    let rowv = Array.make (g.n_cols + 1) 0. in
    let cur_by = ref (-1) in
    (* Per-column edge fade, hoisted out of the inner loop. *)
    let edge_x = Array.make size 0. in
    for j = 0 to size - 1 do
      edge_x.(j) <- float (min j (size - 1 - j)) /. p.fade_x
    done;
    for u = 0 to size - 1 do
      let cby = source_index p p.src_origin_y p.raw_origin_y u in
      let by = int_of_float (floor cby) in
      let fy = cby -. float by in
      if by <> !cur_by then begin
        cur_by := by;
        for k = 0 to g.n_cols - 1 do
          rowa.(k) <- get_base by (g.col_lo + k);
          rowb.(k) <- get_base (by + 1) (g.col_lo + k)
        done
      end;
      for k = 0 to g.n_cols - 1 do
        rowv.(k) <- rowa.(k) +. (fy *. (rowb.(k) -. rowa.(k)))
      done;
      let edge_y = float (min u (size - 1 - u)) /. p.fade_y in
      (* Row 0 of the raster is the northernmost one, row 0 of a DEM tile the
         southernmost. *)
      let src_row = (size - 1 - u) * size in
      let dst_row = u * size * 2 in
      for j = 0 to size - 1 do
        let k = bx.(j) in
        let b = rowv.(k) +. (fx.(j) *. (rowv.(k + 1) -. rowv.(k))) in
        let h = Array1.unsafe_get src (src_row + j) in
        let v =
          if h < nodata_limit then b
          else
            let t = Float.min edge_x.(j) edge_y in
            let t =
              match dist with
              | None -> t
              | Some d ->
                  Float.min t
                    (float (Char.code (Bytes.unsafe_get d (src_row + j)))
                    /. fade_nodata)
            in
            b +. (smoothstep t *. (h -. b))
        in
        let q = int_of_float (((v -. height_offset) *. inv) +. 0.5) in
        let q = if q < 0 then 0 else if q > 65535 then 65535 else q in
        Array1.unsafe_set out (dst_row + (2 * j)) (q land 0xff);
        Array1.unsafe_set out (dst_row + (2 * j) + 1) (q lsr 8)
      done
    done;
    Some { data = out; height_scale; height_offset; range = !hi -. !lo }
  end
