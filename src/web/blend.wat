;; Hand-written port of [Blend_core.run]: one refinement grid mixed into the
;; surface beneath it, entirely on linear memory.
;;
;; Why wat rather than OCaml: bigarray element access is not compiled to a plain
;; load by wasm_of_ocaml today, and this touches some fifty million elements per
;; l13 block. Moving one scratch array from Bigarray to Bytes already took the
;; distance transform from 824 ms to 115; this removes the remaining traffic --
;; 4M float32 reads of the samples and 8M byte writes of the output.
;;
;; Every floating-point expression is written in the same order and grouping as
;; the OCaml, including the odd-looking arithmetic in $index_of, because the two
;; are compared byte for byte on real blocks. f64.min/f64.max match OCaml's
;; Float.min/Float.max exactly (NaN-propagating, min(-0,+0) = -0). The one
;; deliberate difference is i32.trunc_sat_f64_s where OCaml has int_of_float:
;; out-of-range is unspecified in OCaml but traps in wasm, and a trap here would
;; take down the whole render, so it saturates instead -- the values are in range
;; by construction and the quantised result is clamped anyway.
(module
  (import "env" "memory" (memory 1))

  ;; The source window and its quantisation, set once per call so that
  ;; $get_base stays a two-argument call that wasm-opt can inline.
  (global $win_ptr (mut i32) (i32.const 0))
  (global $win_cols (mut i32) (i32.const 0))
  (global $win_last_r (mut i32) (i32.const 0))
  (global $win_last_c (mut i32) (i32.const 0))
  (global $g_row_lo (mut i32) (i32.const 0))
  (global $g_col_lo (mut i32) (i32.const 0))
  (global $g_hs (mut f64) (f64.const 0))
  (global $g_ho (mut f64) (f64.const 0))

  ;; The refinement-to-source mapping.
  (global $g_px (mut f64) (f64.const 0))
  (global $g_spx (mut f64) (f64.const 0))
  (global $g_src_max (mut f64) (f64.const 0)) ;; f64(src_size - 2)

  ;; Height in metres of source sample (row, col), absolute indices, decoded
  ;; with the source's own quantisation. Clamping the indices into the window
  ;; reproduces the clamp to the tile's last row and column that the unwindowed
  ;; form applied: the window ends exactly there whenever it is the tile that
  ;; runs out.
  (func $get_base (param $row i32) (param $col i32) (result f64)
    (local $r i32)
    (local $c i32)
    (local.set $r (i32.sub (local.get $row) (global.get $g_row_lo)))
    (local.set $c (i32.sub (local.get $col) (global.get $g_col_lo)))
    (if (i32.gt_s (local.get $r) (global.get $win_last_r))
      (then (local.set $r (global.get $win_last_r))))
    (if (i32.gt_s (local.get $c) (global.get $win_last_c))
      (then (local.set $c (global.get $win_last_c))))
    (f64.add
      (f64.mul
        (f64.convert_i32_u
          (i32.load16_u
            (i32.add (global.get $win_ptr)
              (i32.shl
                (i32.add (i32.mul (local.get $r) (global.get $win_cols))
                         (local.get $c))
                (i32.const 1)))))
        (global.get $g_hs))
      (global.get $g_ho))
  )

  ;; Fractional source index of refinement sample [j] along one axis, clamped so
  ;; the bilinear pair [i], [i+1] stays inside the grid. Same grouping as the
  ;; OCaml: ((o - origin) + j * px) / spx.
  (func $index_of (param $origin f64) (param $o f64) (param $j i32) (result f64)
    (f64.max (f64.const 0)
      (f64.min (global.get $g_src_max)
        (f64.div
          (f64.add (f64.sub (local.get $o) (local.get $origin))
                   (f64.mul (f64.convert_i32_s (local.get $j))
                            (global.get $g_px)))
          (global.get $g_spx))))
  )

  (func $smoothstep (param $t f64) (result f64)
    (if (f64.le (local.get $t) (f64.const 0)) (then (return (f64.const 0))))
    (if (f64.ge (local.get $t) (f64.const 1)) (then (return (f64.const 1))))
    (f64.mul (f64.mul (local.get $t) (local.get $t))
             (f64.sub (f64.const 3)
                      (f64.mul (f64.const 2) (local.get $t))))
  )

  ;; Chessboard distance in texels to the nearest nodata sample, saturated at
  ;; 255, as two sweeps over the raster. Only called when the block actually
  ;; holds nodata, i.e. near the edge of French coverage.
  (func $nodata_distance (param $samples i32) (param $dist i32)
                         (param $size i32)
    (local $i i32) (local $j i32) (local $row i32) (local $m i32)
    (local $x i32) (local $last i32)
    (local.set $last (i32.sub (local.get $size) (i32.const 1)))
    (memory.fill (local.get $dist) (i32.const 0)
                 (i32.mul (local.get $size) (local.get $size)))

    ;; --- forward sweep: north-west neighbours
    (local.set $i (i32.const 0))
    (loop $fwd_row
      (local.set $row (i32.mul (local.get $i) (local.get $size)))
      (local.set $j (i32.const 0))
      (loop $fwd_col
        (if (f64.lt
              (f64.promote_f32
                (f32.load (i32.add (local.get $samples)
                            (i32.shl (i32.add (local.get $row) (local.get $j))
                                     (i32.const 2)))))
              (f64.const -500))
          (then (local.set $m (i32.const 0)))
          (else
            (local.set $m (i32.const 255))
            (if (i32.gt_s (local.get $i) (i32.const 0))
              (then
                (local.set $x (i32.load8_u
                  (i32.add (local.get $dist)
                    (i32.sub (i32.add (local.get $row) (local.get $j))
                             (local.get $size)))))
                (if (i32.lt_s (local.get $x) (local.get $m))
                  (then (local.set $m (local.get $x))))))
            (if (i32.and (i32.gt_s (local.get $i) (i32.const 0))
                         (i32.gt_s (local.get $j) (i32.const 0)))
              (then
                (local.set $x (i32.load8_u
                  (i32.add (local.get $dist)
                    (i32.sub (i32.sub (i32.add (local.get $row) (local.get $j))
                                      (local.get $size))
                             (i32.const 1)))))
                (if (i32.lt_s (local.get $x) (local.get $m))
                  (then (local.set $m (local.get $x))))))
            (if (i32.and (i32.gt_s (local.get $i) (i32.const 0))
                         (i32.lt_s (local.get $j) (local.get $last)))
              (then
                (local.set $x (i32.load8_u
                  (i32.add (local.get $dist)
                    (i32.add (i32.sub (i32.add (local.get $row) (local.get $j))
                                      (local.get $size))
                             (i32.const 1)))))
                (if (i32.lt_s (local.get $x) (local.get $m))
                  (then (local.set $m (local.get $x))))))
            (if (i32.gt_s (local.get $j) (i32.const 0))
              (then
                (local.set $x (i32.load8_u
                  (i32.sub (i32.add (local.get $dist)
                             (i32.add (local.get $row) (local.get $j)))
                           (i32.const 1))))
                (if (i32.lt_s (local.get $x) (local.get $m))
                  (then (local.set $m (local.get $x))))))
            ;; saturate: 255 stays 255, anything else grows by one
            (if (i32.lt_s (local.get $m) (i32.const 255))
              (then (local.set $m (i32.add (local.get $m) (i32.const 1)))))))
        (i32.store8 (i32.add (local.get $dist)
                      (i32.add (local.get $row) (local.get $j)))
                    (local.get $m))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br_if $fwd_col (i32.lt_u (local.get $j) (local.get $size))))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (br_if $fwd_row (i32.lt_u (local.get $i) (local.get $size))))

    ;; --- backward sweep: south-east neighbours, each one step further away
    (local.set $i (local.get $last))
    (block $bwd_done
      (loop $bwd_row
        (br_if $bwd_done (i32.lt_s (local.get $i) (i32.const 0)))
        (local.set $row (i32.mul (local.get $i) (local.get $size)))
        (local.set $j (local.get $last))
        (block $bwd_col_done
          (loop $bwd_col
            (br_if $bwd_col_done (i32.lt_s (local.get $j) (i32.const 0)))
            (local.set $m (i32.load8_u
              (i32.add (local.get $dist)
                       (i32.add (local.get $row) (local.get $j)))))
            (if (i32.gt_s (local.get $m) (i32.const 0))
              (then
                (if (i32.lt_s (local.get $i) (local.get $last))
                  (then
                    (local.set $x (i32.add (i32.load8_u
                      (i32.add (local.get $dist)
                        (i32.add (i32.add (local.get $row) (local.get $size))
                                 (local.get $j)))) (i32.const 1)))
                    (if (i32.lt_s (local.get $x) (local.get $m))
                      (then (local.set $m (local.get $x))))))
                (if (i32.and (i32.lt_s (local.get $i) (local.get $last))
                             (i32.gt_s (local.get $j) (i32.const 0)))
                  (then
                    (local.set $x (i32.add (i32.load8_u
                      (i32.sub (i32.add (local.get $dist)
                                 (i32.add (i32.add (local.get $row)
                                                   (local.get $size))
                                          (local.get $j)))
                               (i32.const 1))) (i32.const 1)))
                    (if (i32.lt_s (local.get $x) (local.get $m))
                      (then (local.set $m (local.get $x))))))
                (if (i32.and (i32.lt_s (local.get $i) (local.get $last))
                             (i32.lt_s (local.get $j) (local.get $last)))
                  (then
                    (local.set $x (i32.add (i32.load8_u
                      (i32.add (i32.add (local.get $dist)
                                 (i32.add (i32.add (local.get $row)
                                                   (local.get $size))
                                          (local.get $j)))
                               (i32.const 1))) (i32.const 1)))
                    (if (i32.lt_s (local.get $x) (local.get $m))
                      (then (local.set $m (local.get $x))))))
                (if (i32.lt_s (local.get $j) (local.get $last))
                  (then
                    (local.set $x (i32.add (i32.load8_u
                      (i32.add (i32.add (local.get $dist)
                                 (i32.add (local.get $row) (local.get $j)))
                               (i32.const 1))) (i32.const 1)))
                    (if (i32.lt_s (local.get $x) (local.get $m))
                      (then (local.set $m (local.get $x))))))
                (i32.store8 (i32.add (local.get $dist)
                              (i32.add (local.get $row) (local.get $j)))
                            (local.get $m))))
            (local.set $j (i32.sub (local.get $j) (i32.const 1)))
            (br $bwd_col)))
        (local.set $i (i32.sub (local.get $i) (i32.const 1)))
        (br $bwd_row)))
  )

  ;; Returns 1 when the block held at least one valid sample, 0 otherwise (a
  ;; location outside French coverage, which must stay on the source alone).
  ;; On success writes three f64 at $result: height_scale, height_offset, range.
  ;;
  ;; $aux is scratch, carved up below; it needs
  ;;   size * 20 + (n_cols + 1) * 24 bytes.
  ;; $dist needs size * size bytes, and is only touched when there is nodata.
  (func $blend (export "blend")
    (param $samples i32)  ;; f32[size * size], north-up, metres
    (param $win i32)      ;; u16[win_rows * win_cols], source window
    (param $out i32)      ;; u8[size * size * 2], result, row 0 southernmost
    (param $dist i32)
    (param $aux i32)
    (param $result i32)
    (param $size i32)
    (param $src_size i32)
    (param $win_cols_a i32)
    (param $win_rows_a i32)
    (param $col_lo i32)
    (param $row_lo i32)
    (param $n_cols i32)
    (param $n_rows i32)
    (param $px f64)
    (param $spx f64)
    (param $rox f64)
    (param $roy f64)
    (param $sox f64)
    (param $soy f64)
    (param $shs f64)
    (param $sho f64)
    (param $fade_x f64)
    (param $fade_y f64)
    (result i32)

    (local $n i32) (local $i i32) (local $j i32) (local $k i32) (local $u i32)
    (local $last i32) (local $mj i32) (local $t2 i32)
    (local $has_nodata i32) (local $has_data i32)
    (local $by i32) (local $cur_by i32) (local $q i32)
    (local $bx_p i32) (local $fx_p i32) (local $ex_p i32)
    (local $ra_p i32) (local $rb_p i32) (local $rv_p i32)
    (local $rk i32) (local $src_row i32) (local $dst i32)
    (local $lo f64) (local $hi f64) (local $h f64) (local $a f64)
    (local $c f64) (local $bfl f64) (local $cby f64) (local $fy f64)
    (local $edge_y f64) (local $t f64) (local $vf f64) (local $bh f64)
    (local $hs f64) (local $ho f64) (local $inv f64) (local $fade_nd f64)

    (global.set $win_ptr (local.get $win))
    (global.set $win_cols (local.get $win_cols_a))
    (global.set $win_last_r (i32.sub (local.get $win_rows_a) (i32.const 1)))
    (global.set $win_last_c (i32.sub (local.get $win_cols_a) (i32.const 1)))
    (global.set $g_row_lo (local.get $row_lo))
    (global.set $g_col_lo (local.get $col_lo))
    (global.set $g_hs (local.get $shs))
    (global.set $g_ho (local.get $sho))
    (global.set $g_px (local.get $px))
    (global.set $g_spx (local.get $spx))
    (global.set $g_src_max
      (f64.convert_i32_s (i32.sub (local.get $src_size) (i32.const 2))))

    (local.set $n (i32.mul (local.get $size) (local.get $size)))
    (local.set $last (i32.sub (local.get $size) (i32.const 1)))

    ;; ---- range of the refinement, and whether it holds data or nodata at all
    (local.set $lo (f64.const inf))
    (local.set $hi (f64.const -inf))
    (local.set $i (i32.const 0))
    (block $scan_done
      (loop $scan
        (br_if $scan_done (i32.ge_u (local.get $i) (local.get $n)))
        (local.set $h (f64.promote_f32
          (f32.load (i32.add (local.get $samples)
                             (i32.shl (local.get $i) (i32.const 2))))))
        (if (f64.lt (local.get $h) (f64.const -500))
          (then (local.set $has_nodata (i32.const 1)))
          (else
            (local.set $has_data (i32.const 1))
            (if (f64.lt (local.get $h) (local.get $lo))
              (then (local.set $lo (local.get $h))))
            (if (f64.gt (local.get $h) (local.get $hi))
              (then (local.set $hi (local.get $h))))))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $scan)))
    (if (i32.eqz (local.get $has_data)) (then (return (i32.const 0))))

    (if (local.get $has_nodata)
      (then (call $nodata_distance (local.get $samples) (local.get $dist)
                                   (local.get $size))))
    (local.set $fade_nd (f64.min (local.get $fade_x) (local.get $fade_y)))

    ;; ---- scratch layout
    (local.set $bx_p (local.get $aux))
    (local.set $fx_p (i32.add (local.get $bx_p)
                       (i32.shl (local.get $size) (i32.const 2))))
    (local.set $ex_p (i32.add (local.get $fx_p)
                       (i32.shl (local.get $size) (i32.const 3))))
    (local.set $ra_p (i32.add (local.get $ex_p)
                       (i32.shl (local.get $size) (i32.const 3))))
    (local.set $rb_p (i32.add (local.get $ra_p)
                       (i32.shl (i32.add (local.get $n_cols) (i32.const 1))
                                (i32.const 3))))
    (local.set $rv_p (i32.add (local.get $rb_p)
                       (i32.shl (i32.add (local.get $n_cols) (i32.const 1))
                                (i32.const 3))))

    ;; ---- per-column source index, fraction and edge fade
    (local.set $j (i32.const 0))
    (loop $pre
      (local.set $c (call $index_of (local.get $sox) (local.get $rox)
                                    (local.get $j)))
      (local.set $bfl (f64.floor (local.get $c)))
      (i32.store (i32.add (local.get $bx_p)
                   (i32.shl (local.get $j) (i32.const 2)))
                 (i32.sub (i32.trunc_sat_f64_s (local.get $bfl))
                          (local.get $col_lo)))
      (f64.store (i32.add (local.get $fx_p)
                   (i32.shl (local.get $j) (i32.const 3)))
                 (f64.sub (local.get $c) (local.get $bfl)))
      (local.set $mj (local.get $j))
      (local.set $t2 (i32.sub (local.get $last) (local.get $j)))
      (if (i32.lt_s (local.get $t2) (local.get $mj))
        (then (local.set $mj (local.get $t2))))
      (f64.store (i32.add (local.get $ex_p)
                   (i32.shl (local.get $j) (i32.const 3)))
                 (f64.div (f64.convert_i32_s (local.get $mj))
                          (local.get $fade_x)))
      (local.set $j (i32.add (local.get $j) (i32.const 1)))
      (br_if $pre (i32.lt_u (local.get $j) (local.get $size))))

    ;; ---- widen the range over the source window, bounding the output rather
    ;; than measuring it: every blended value lies on the segment between the
    ;; two surfaces, hence inside the union of their ranges.
    (local.set $j (i32.const 0))
    (block $rs_done
      (loop $rs_row
        (br_if $rs_done (i32.gt_s (local.get $j) (local.get $n_rows)))
        (local.set $i (i32.const 0))
        (block $rs_col_done
          (loop $rs_col
            (br_if $rs_col_done (i32.gt_s (local.get $i) (local.get $n_cols)))
            (local.set $bh (call $get_base
              (i32.add (local.get $row_lo) (local.get $j))
              (i32.add (local.get $col_lo) (local.get $i))))
            (if (f64.lt (local.get $bh) (local.get $lo))
              (then (local.set $lo (local.get $bh))))
            (if (f64.gt (local.get $bh) (local.get $hi))
              (then (local.set $hi (local.get $bh))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $rs_col)))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br $rs_row)))

    (local.set $ho (local.get $lo))
    (local.set $hs (f64.max (f64.const 1e-6)
      (f64.div (f64.sub (local.get $hi) (local.get $lo)) (f64.const 65535))))
    (local.set $inv (f64.div (f64.const 1) (local.get $hs)))

    ;; ---- resample, fade, quantise
    (local.set $cur_by (i32.const -1))
    (local.set $u (i32.const 0))
    (loop $row_loop
      (local.set $cby (call $index_of (local.get $soy) (local.get $roy)
                                      (local.get $u)))
      (local.set $by (i32.trunc_sat_f64_s (f64.floor (local.get $cby))))
      (local.set $fy (f64.sub (local.get $cby)
                              (f64.convert_i32_s (local.get $by))))
      ;; One source row pair per source row, not per sample.
      (if (i32.ne (local.get $by) (local.get $cur_by))
        (then
          (local.set $cur_by (local.get $by))
          (local.set $k (i32.const 0))
          (block $fill_done
            (loop $fill
              (br_if $fill_done (i32.ge_s (local.get $k) (local.get $n_cols)))
              (f64.store (i32.add (local.get $ra_p)
                           (i32.shl (local.get $k) (i32.const 3)))
                         (call $get_base (local.get $by)
                           (i32.add (local.get $col_lo) (local.get $k))))
              (f64.store (i32.add (local.get $rb_p)
                           (i32.shl (local.get $k) (i32.const 3)))
                         (call $get_base
                           (i32.add (local.get $by) (i32.const 1))
                           (i32.add (local.get $col_lo) (local.get $k))))
              (local.set $k (i32.add (local.get $k) (i32.const 1)))
              (br $fill)))))
      (local.set $k (i32.const 0))
      (block $rv_done
        (loop $rv
          (br_if $rv_done (i32.ge_s (local.get $k) (local.get $n_cols)))
          (local.set $a (f64.load (i32.add (local.get $ra_p)
                                    (i32.shl (local.get $k) (i32.const 3)))))
          (f64.store (i32.add (local.get $rv_p)
                       (i32.shl (local.get $k) (i32.const 3)))
                     (f64.add (local.get $a)
                       (f64.mul (local.get $fy)
                         (f64.sub (f64.load (i32.add (local.get $rb_p)
                                    (i32.shl (local.get $k) (i32.const 3))))
                                  (local.get $a)))))
          (local.set $k (i32.add (local.get $k) (i32.const 1)))
          (br $rv)))

      (local.set $mj (local.get $u))
      (local.set $t2 (i32.sub (local.get $last) (local.get $u)))
      (if (i32.lt_s (local.get $t2) (local.get $mj))
        (then (local.set $mj (local.get $t2))))
      (local.set $edge_y (f64.div (f64.convert_i32_s (local.get $mj))
                                  (local.get $fade_y)))
      ;; Row 0 of the raster is the northernmost, row 0 of a DEM tile the
      ;; southernmost.
      (local.set $src_row (i32.mul (i32.sub (local.get $last) (local.get $u))
                                   (local.get $size)))
      (local.set $dst (i32.add (local.get $out)
                        (i32.shl (i32.mul (local.get $u) (local.get $size))
                                 (i32.const 1))))
      (local.set $j (i32.const 0))
      (loop $col_loop
        (local.set $rk (i32.add (local.get $rv_p)
          (i32.shl (i32.load (i32.add (local.get $bx_p)
                               (i32.shl (local.get $j) (i32.const 2))))
                   (i32.const 3))))
        (local.set $a (f64.load (local.get $rk)))
        (local.set $bh (f64.add (local.get $a)
          (f64.mul (f64.load (i32.add (local.get $fx_p)
                               (i32.shl (local.get $j) (i32.const 3))))
                   (f64.sub (f64.load offset=8 (local.get $rk))
                            (local.get $a)))))
        (local.set $h (f64.promote_f32
          (f32.load (i32.add (local.get $samples)
                      (i32.shl (i32.add (local.get $src_row) (local.get $j))
                               (i32.const 2))))))
        (if (f64.lt (local.get $h) (f64.const -500))
          (then (local.set $vf (local.get $bh)))
          (else
            (local.set $t (f64.min
              (f64.load (i32.add (local.get $ex_p)
                          (i32.shl (local.get $j) (i32.const 3))))
              (local.get $edge_y)))
            (if (local.get $has_nodata)
              (then
                (local.set $t (f64.min (local.get $t)
                  (f64.div
                    (f64.convert_i32_u (i32.load8_u
                      (i32.add (local.get $dist)
                        (i32.add (local.get $src_row) (local.get $j)))))
                    (local.get $fade_nd))))))
            (local.set $vf (f64.add (local.get $bh)
              (f64.mul (call $smoothstep (local.get $t))
                       (f64.sub (local.get $h) (local.get $bh)))))))
        (local.set $q (i32.trunc_sat_f64_s
          (f64.add (f64.mul (f64.sub (local.get $vf) (local.get $ho))
                            (local.get $inv))
                   (f64.const 0.5))))
        (if (i32.lt_s (local.get $q) (i32.const 0))
          (then (local.set $q (i32.const 0))))
        (if (i32.gt_s (local.get $q) (i32.const 65535))
          (then (local.set $q (i32.const 65535))))
        (i32.store16 (i32.add (local.get $dst)
                       (i32.shl (local.get $j) (i32.const 1)))
                     (local.get $q))
        (local.set $j (i32.add (local.get $j) (i32.const 1)))
        (br_if $col_loop (i32.lt_u (local.get $j) (local.get $size))))
      (local.set $u (i32.add (local.get $u) (i32.const 1)))
      (br_if $row_loop (i32.lt_u (local.get $u) (local.get $size))))

    (f64.store (local.get $result) (local.get $hs))
    (f64.store offset=8 (local.get $result) (local.get $ho))
    (f64.store offset=16 (local.get $result)
               (f64.sub (local.get $hi) (local.get $lo)))
    (i32.const 1)
  )
)
