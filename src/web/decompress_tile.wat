(module
  ;; --------------------------------------------------------------------------
  ;; Imports: Three distinct memories + 1 Internal
  ;; --------------------------------------------------------------------------
  (import "env" "high_mem" (memory $m_high 1))
  (import "env" "low_mem" (memory $m_low 1))
  (import "env" "target_mem" (memory $m_target 1))

  ;; Internal Scratch Memory (Line Buffer)
  (memory $m_scratch 1)

  ;; --------------------------------------------------------------------------
  ;; Exported Function: decompress_simd
  ;; --------------------------------------------------------------------------
  (func $decompress_simd (export "decompress_simd")
    (param $high_ptr i32)
    (param $low_ptr i32)
    (param $target_base i32)
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

    (i32.const 0) ;; offset
    (i32.const 0) ;; value
    (local.get $block_w)
    (i32.const 1)
    (i32.shl)     ;; count (bytes)
    (memory.fill $m_scratch)

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
        (local.set $v_lo (v128.load8x8_u $m_low (local.get $low_ptr)))
        (local.set $low_ptr (i32.add (local.get $low_ptr) (i32.const 8)))

        (local.set $v_hi (v128.load8x8_u $m_high (local.get $high_ptr)))
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
        (local.set $v_prev (v128.load $m_scratch (i32.shl (local.get $x) (i32.const 1))))
        (local.set $v_res (i16x8.add (local.get $v_res) (local.get $v_prev)))

        ;; Update Scratch
        (v128.store $m_scratch (i32.shl (local.get $x) (i32.const 1)) (local.get $v_res))

        (local.set $x (i32.add (local.get $x) (i32.const 8)))
        (br_if $loop_x (i32.lt_u (local.get $x) (local.get $block_w)))
      )

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
           (memory.copy $m_target $m_scratch
              (local.get $dst_off)
              (local.get $src_off)
              (i32.shl (local.get $c_len) (i32.const 1)) ;; Bytes
           )
        )
      )

      (local.set $y (i32.add (local.get $y) (i32.const 1)))
      ;; Loop while y < limit_y (computed as min(block_h, target_h - start_row))
      (br_if $loop_y (i32.lt_u (local.get $y) (local.get $limit_y)))
    )
  )
)
