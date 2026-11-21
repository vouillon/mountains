(module
  ;; --------------------------------------------------------------------------
  ;; Imports: Single Shared Memory
  ;; --------------------------------------------------------------------------
  (import "env" "memory" (memory 1))

  ;; --------------------------------------------------------------------------
  ;; Exported Function: decompress_simd
  ;; --------------------------------------------------------------------------
  (func $decompress_simd (export "decompress_simd")
    (param $high_ptr i32)
    (param $low_ptr i32)
    (param $target_base i32)
    (param $scratch_ptr i32)
    (param $start_col i32)
    (param $start_row i32)
    (param $target_w i32)
    (param $target_h i32)
    (param $stride i32)
    (param $block_w i32)
    (param $block_h i32)

    ;; --- Locals ---
    (local $y i32)
    (local $x i32)
    (local $abs_row i32)
    (local $limit_y i32)      ;; New: Calculated loop limit

    ;; Predictors
    (local $run_scalar i32)
    (local $run_vec v128)

    ;; SIMD Vars
    (local $v_lo v128)
    (local $v_hi v128)
    (local $v_raw v128)
    (local $v_res v128)
    (local $v_shuf v128)
    (local $v_prev v128)
    (local $zero v128)
    (local $one v128)

    ;; Copy/Clip Calculation Vars
    (local $c_start i32)
    (local $c_end i32)
    (local $c_len i32)
    (local $src_off i32)
    (local $dst_off i32)

    (local.set $zero (v128.const i16x8 0 0 0 0 0 0 0 0))
    (local.set $one (v128.const i16x8 1 1 1 1 1 1 1 1))

    (local.get $scratch_ptr) ;; offset
    (i32.const 0) ;; value
    (local.get $block_w)
    (i32.const 1)
    (i32.shl)     ;; count (bytes)
    (memory.fill)

    ;; ------------------------------------------------------------------------
    ;; Optimization: Pre-calculate Intersection [start_col, start_col+block]
    ;; ------------------------------------------------------------------------
    ;; Calculate c_start = max(start_col, 0)
    (local.set $c_start (local.get $start_col))
    (if (i32.lt_s (local.get $c_start) (i32.const 0)) (then (local.set $c_start (i32.const 0))))

    ;; Calculate c_end = min(start_col + block_w, target_w)
    (local.set $c_end (i32.add (local.get $start_col) (local.get $block_w)))
    (if (i32.gt_s (local.get $c_end) (local.get $target_w)) (then (local.set $c_end (local.get $target_w))))

    ;; Calculate length
    (local.set $c_len (i32.sub (local.get $c_end) (local.get $c_start)))

    ;; Calculate constant Scratch Source Offset: (c_start - start_col) * 2
    (local.set $src_off (i32.sub (local.get $c_start) (local.get $start_col)))
    (local.set $src_off (i32.shl (local.get $src_off) (i32.const 1)))

    ;; ------------------------------------------------------------------------
    ;; Optimization: Pre-calculate Y Limit
    ;; ------------------------------------------------------------------------
    ;; limit_y = min(block_h, target_h - start_row)
    (local.set $limit_y (local.get $block_h))

    ;; Check if (target_h - start_row) is smaller
    (if (i32.lt_s (i32.sub (local.get $target_h) (local.get $start_row)) (local.get $limit_y))
        (then
            (local.set $limit_y (i32.sub (local.get $target_h) (local.get $start_row)))
        )
    )

    ;; If limit_y <= 0, the block is fully below the image (or invalid). Return immediately.
    (if (i32.le_s (local.get $limit_y) (i32.const 0)) (then (return)))

    ;; ------------------------------------------------------------------------
    ;; PHASE 1: Main Row Loop
    ;; ------------------------------------------------------------------------
    (local.set $y (i32.const 0))
    (loop $loop_y

      ;; Reset Horizontal Predictor
      (local.set $run_scalar (i32.const 0))
      (local.set $run_vec (v128.const i16x8 0 0 0 0 0 0 0 0))

      ;; --- Inner Loop: Decode to Scratch ---
      (local.set $x (i32.const 0))
      (loop $loop_x
        ;; Load & Merge Streams
        (local.set $v_lo (v128.load8x8_u (local.get $low_ptr)))
        (local.set $low_ptr (i32.add (local.get $low_ptr) (i32.const 8)))

        (local.set $v_hi (v128.load8x8_u (local.get $high_ptr)))
        (local.set $v_hi (i16x8.shl (local.get $v_hi) (i32.const 8)))
        (local.set $high_ptr (i32.add (local.get $high_ptr) (i32.const 8)))

        (local.set $v_raw (v128.or (local.get $v_lo) (local.get $v_hi)))

        ;; Zigzag
        (local.set $v_res (i16x8.shr_u (local.get $v_raw) (i32.const 1)))
        (local.set $v_shuf (v128.and (local.get $v_raw) (local.get $one)))
        (local.set $v_shuf (i16x8.sub (local.get $zero) (local.get $v_shuf)))
        (local.set $v_res (v128.xor (local.get $v_res) (local.get $v_shuf)))

        ;; Horizontal Prefix Sum
        (local.set $v_res 
          (i16x8.add (local.get $v_res)
            (i8x16.shuffle 16 17 0 1 2 3 4 5 6 7 8 9 10 11 12 13 (local.get $v_res) (local.get $zero))))

        (local.set $v_res 
          (i16x8.add (local.get $v_res)
            (i8x16.shuffle 16 17 18 19 0 1 2 3 4 5 6 7 8 9 10 11 (local.get $v_res) (local.get $zero))))

        (local.set $v_res 
          (i16x8.add (local.get $v_res)
            (i8x16.shuffle 16 17 18 19 20 21 22 23 0 1 2 3 4 5 6 7 (local.get $v_res) (local.get $zero))))

        (local.set $v_res (i16x8.add (local.get $v_res) (local.get $run_vec)))

        (local.set $run_scalar (i16x8.extract_lane_u 7 (local.get $v_res)))
        (local.set $run_vec (i16x8.splat (local.get $run_scalar)))

        ;; Vertical Predict
        (local.set $v_prev (v128.load (i32.add (local.get $scratch_ptr) (i32.shl (local.get $x) (i32.const 1)))))
        (local.set $v_res (i16x8.add (local.get $v_res) (local.get $v_prev)))

        ;; Update Scratch
        (v128.store (i32.add (local.get $scratch_ptr) (i32.shl (local.get $x) (i32.const 1))) (local.get $v_res))

        (local.set $x (i32.add (local.get $x) (i32.const 8)))
        (br_if $loop_x (i32.lt_u (local.get $x) (local.get $block_w)))
      )

      ;; A row whose width is not a multiple of 8 was read past its end, into
      ;; the next one (the far field's tiles are 300 wide): step back. The
      ;; lanes decoded past the end land in the scratch row beyond block_w and
      ;; never reach the target, nor the lanes before them.
      (local.set $low_ptr
        (i32.sub (local.get $low_ptr) (i32.sub (local.get $x) (local.get $block_w))))
      (local.set $high_ptr
        (i32.sub (local.get $high_ptr) (i32.sub (local.get $x) (local.get $block_w))))

      ;; --- Flush Line to Target (Clipping Check) ---
      (local.set $abs_row (i32.add (local.get $start_row) (local.get $y)))

      ;; We only check for the TOP edge (abs_row >= 0) and horizontal validity (c_len > 0).
      ;; The bottom edge is handled by the loop limit $limit_y.
      (if (i32.and
             (i32.ge_s (local.get $abs_row) (i32.const 0))
             (i32.gt_s (local.get $c_len) (i32.const 0))
          )
        (then
           ;; Calculate Destination Address
           (local.set $dst_off (i32.add (i32.mul (local.get $abs_row) (local.get $stride)) (local.get $c_start)))
           (local.set $dst_off (i32.shl (local.get $dst_off) (i32.const 1)))
           (local.set $dst_off (i32.add (local.get $dst_off) (local.get $target_base)))

           ;; Copy using pre-calculated length and src_off
           (memory.copy
              (local.get $dst_off)
              (i32.add (local.get $scratch_ptr) (local.get $src_off))
              (i32.shl (local.get $c_len) (i32.const 1)) ;; Bytes
           )
        )
      )

      (local.set $y (i32.add (local.get $y) (i32.const 1)))
      ;; Loop while y < limit_y (computed as min(block_h, target_h - start_row))
      (br_if $loop_y (i32.lt_u (local.get $y) (local.get $limit_y)))
    )
  )

  ;; --------------------------------------------------------------------------
  ;; Exported Function: scale_hdt
  ;; --------------------------------------------------------------------------
  ;; An HDT1 tile's heights (Hd_dem.Own_tiles): the [n] u16 samples at [src],
  ;; [n] a multiple of 8, written as float32 at [dst], [base + step * u], and
  ;; -99999 for the nodata sample 65535. Computed in f64 and rounded once to
  ;; f32, as the JavaScript loop it replaces did: the same heights to the bit.
  (func $scale_hdt (export "scale_hdt")
    (param $src i32)
    (param $dst i32)
    (param $n i32)
    (param $base f64)
    (param $step f64)
    (local $i i32)
    (local $v v128)
    (local $q v128)
    (local $b2 v128)
    (local $s2 v128)
    (local $nodata v128)
    (local $none v128)
    (local.set $b2 (f64x2.splat (local.get $base)))
    (local.set $s2 (f64x2.splat (local.get $step)))
    (local.set $nodata (i32x4.splat (i32.const 65535)))
    (local.set $none (f32x4.splat (f32.const -99999)))
    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
        (local.set $v
          (v128.load (i32.add (local.get $src) (i32.shl (local.get $i) (i32.const 1)))))
        ;; Samples 0-3, then 4-7.
        (local.set $q (i32x4.extend_low_i16x8_u (local.get $v)))
        (v128.store
          (i32.add (local.get $dst) (i32.shl (local.get $i) (i32.const 2)))
          (call $scale4 (local.get $q) (local.get $b2) (local.get $s2)
            (local.get $nodata) (local.get $none)))
        (local.set $q (i32x4.extend_high_i16x8_u (local.get $v)))
        (v128.store
          (i32.add (local.get $dst)
            (i32.shl (i32.add (local.get $i) (i32.const 4)) (i32.const 2)))
          (call $scale4 (local.get $q) (local.get $b2) (local.get $s2)
            (local.get $nodata) (local.get $none)))
        (local.set $i (i32.add (local.get $i) (i32.const 8)))
        (br $loop)
      )
    )
  )

  ;; Four u32 samples to four float32 heights (see scale_hdt).
  (func $scale4
    (param $q v128) (param $b2 v128) (param $s2 v128)
    (param $nodata v128) (param $none v128)
    (result v128)
    (local $lo v128)
    (local $hi v128)
    (local.set $lo
      (f32x4.demote_f64x2_zero
        (f64x2.add (local.get $b2)
          (f64x2.mul (local.get $s2) (f64x2.convert_low_i32x4_u (local.get $q))))))
    (local.set $hi
      (f32x4.demote_f64x2_zero
        (f64x2.add (local.get $b2)
          (f64x2.mul (local.get $s2)
            (f64x2.convert_low_i32x4_u
              (i8x16.shuffle 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15
                (local.get $q) (local.get $q)))))))
    (v128.bitselect
      (local.get $none)
      (i8x16.shuffle 0 1 2 3 4 5 6 7 16 17 18 19 20 21 22 23
        (local.get $lo) (local.get $hi))
      (i32x4.eq (local.get $q) (local.get $nodata)))
  )
)
