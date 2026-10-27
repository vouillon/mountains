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
  to_src : Affine.t;
      (* refinement sample index to fractional source sample index. One map
         rather than an origin and a spacing per axis, because a refinement
         served in its own projected CRS sits on a grid whose axes are turned
         from the source's -- see [Hd_dem.frame]. The graticule-aligned case is
         this with [b] and [d] zero, which is what [Blend_wasm] tests for. *)
  src_size : int; (* samples per side of the surface beneath *)
  src_height_scale : float; (* metres per u16 step of the source *)
  src_height_offset : float; (* metres at source u16 zero *)
  fade_x : float; (* fade width, in refinement samples, per axis *)
  fade_y : float;
}

(* Fractional source index of refinement sample [(j, u)], clamped so the
   bilinear pair [i], [i + 1] stays inside the grid on both axes. The two grids
   need not be power-of-two multiples of one another, or even share axes. *)
let source_index p j u =
  let c, r = Affine.apply p.to_src (float j) (float u) in
  let lim = float (p.src_size - 2) in
  (Float.max 0. (Float.min lim c), Float.max 0. (Float.min lim r))

(* True when columns depend only on [j] and rows only on [u], so the resampling
   loop can hoist one source row pair per source row and precompute the column
   indices once for the whole grid. Every blend but one is like this: the base
   and l13 are on the graticule, and two rings in the same projected CRS share
   axes with each other. *)
let axis_aligned p = p.to_src.Affine.b = 0. && p.to_src.Affine.d = 0.

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
  (* An affine map takes a rectangle to a parallelogram, whose extreme
     coordinates are attained at the corners; the clamp in [source_index] is
     monotone, so it cannot move an interior sample outside the corners' box
     either. Four corners therefore bound the whole grid, turned or not. *)
  let e = p.size - 1 in
  let corners =
    [
      source_index p 0 0;
      source_index p e 0;
      source_index p 0 e;
      source_index p e e;
    ]
  in
  let extent f =
    ( List.fold_left (fun a c -> Float.min a (f c)) infinity corners,
      List.fold_left (fun a c -> Float.max a (f c)) neg_infinity corners )
  in
  let col_min, col_max = extent fst and row_min, row_max = extent snd in
  let col_lo = int_of_float (floor col_min) in
  let col_hi = int_of_float (floor col_max) + 1 in
  let row_lo = int_of_float (floor row_min) in
  let row_hi = int_of_float (floor row_max) + 1 in
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
    let col_of j = fst (source_index p j 0) in
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
    (* Per-column edge fade, hoisted out of the inner loop. *)
    let edge_x = Array.make size 0. in
    for j = 0 to size - 1 do
      edge_x.(j) <- float (min j (size - 1 - j)) /. p.fade_x
    done;
    (* The fade and the store, shared by the two resampling loops below so there
       is one copy of them however the source is addressed. *)
    let emit ~b ~ex ~edge_y ~src_row ~dst_row j =
      let h = Array1.unsafe_get src (src_row + j) in
      let v =
        if h < nodata_limit then b
        else
          let t = Float.min ex edge_y in
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
    in
    (* Row 0 of the raster is the northernmost one, row 0 of a DEM tile the
       southernmost. *)
    let src_row u = (size - 1 - u) * size and dst_row u = u * size * 2 in
    let edge_y u = float (min u (size - 1 - u)) /. p.fade_y in
    if axis_aligned p then begin
      (* One base row pair resampled per base row rather than per sample, and
         the column indices from the precomputed table. This is the arithmetic
         [blend.wat] reproduces, so it is written the way the wat is. *)
      let rowa = Array.make (g.n_cols + 1) 0. in
      let rowb = Array.make (g.n_cols + 1) 0. in
      let rowv = Array.make (g.n_cols + 1) 0. in
      let cur_by = ref (-1) in
      for u = 0 to size - 1 do
        let cby = snd (source_index p 0 u) in
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
        let edge_y = edge_y u and src_row = src_row u and dst_row = dst_row u in
        for j = 0 to size - 1 do
          let k = bx.(j) in
          let b = rowv.(k) +. (fx.(j) *. (rowv.(k + 1) -. rowv.(k))) in
          emit ~b ~ex:edge_x.(j) ~edge_y ~src_row ~dst_row j
        done
      done
    end
    else
      (* Turned axes: the source row under a sample changes along the row, so
         there is no row pair to hoist and every sample pays a full bilinear.
         Only one blend in the chain is like this -- the outermost ring on a
         projected grid over the graticule-aligned surface beneath it -- and it
         is the smaller of the two projected ones. *)
      for u = 0 to size - 1 do
        let edge_y = edge_y u and src_row = src_row u and dst_row = dst_row u in
        for j = 0 to size - 1 do
          let c, r = source_index p j u in
          let fc = floor c and fr = floor r in
          let ic = int_of_float fc and ir = int_of_float fr in
          let tc = c -. fc and tr = r -. fr in
          let h00 = get_base ir ic and h10 = get_base ir (ic + 1) in
          let h01 = get_base (ir + 1) ic and h11 = get_base (ir + 1) (ic + 1) in
          let b0 = h00 +. (tc *. (h10 -. h00)) in
          let b1 = h01 +. (tc *. (h11 -. h01)) in
          let b = b0 +. (tr *. (b1 -. b0)) in
          emit ~b ~ex:edge_x.(j) ~edge_y ~src_row ~dst_row j
        done
      done;
    Some { data = out; height_scale; height_offset; range = !hi -. !lo }
  end
