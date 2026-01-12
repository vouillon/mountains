(module
  ;; Memory imports for input streams
  (memory $m_meta (import "env" "m_meta") 1)
  (memory $m_hi_x (import "env" "m_hi_x") 1)
  (memory $m_mid_x (import "env" "m_mid_x") 1)
  (memory $m_lo_x (import "env" "m_lo_x") 1)
  (memory $m_hi_y (import "env" "m_hi_y") 1)
  (memory $m_mid_y (import "env" "m_mid_y") 1)
  (memory $m_lo_y (import "env" "m_lo_y") 1)
  (memory $m_hi_i (import "env" "m_hi_i") 1)
  (memory $m_lo_i (import "env" "m_lo_i") 1)

  ;; Memory imports for output arrays
  (memory $m_out_pos (import "env" "m_out_pos") 1)
  (memory $m_out_col (import "env" "m_out_col") 1)
  (memory $m_out_ebo (import "env" "m_out_ebo") 1)
  (memory $m_out_wpos (import "env" "m_out_wpos") 1)
  (memory $m_palette (import "env" "m_palette") 1)

  ;; Constants
  (global $one v128 (v128.const i16x8 1 1 1 1 1 1 1 1))
  (global $zero v128 (v128.const i16x8 0 0 0 0 0 0 0 0))
  (global $mask24 i32 (i32.const 0xFFFFFF))

  ;; Zigzag decode SIMD: (n >> 1) ^ -(n & 1)
  (func $zigzag_simd (param $v v128) (result v128)
    (local $v_res v128)
    (local $v_shuf v128)
    (local.set $v_res (i16x8.shr_u (local.get $v) (i32.const 1)))
    (local.set $v_shuf (v128.and (local.get $v) (global.get $one)))
    (local.set $v_shuf (i16x8.sub (global.get $zero) (local.get $v_shuf)))
    (v128.xor (local.get $v_res) (local.get $v_shuf))
  )

  ;; Scalar zigzag for i32
  (func $zigzag_i32 (param $n i32) (result i32)
    (i32.xor (i32.shr_u (local.get $n) (i32.const 1)) (i32.sub (i32.const 0) (i32.and (local.get $n) (i32.const 1))))
  )

  ;; Prefix sum SIMD for i16x8
  (func $prefix_sum_simd (param $v v128) (param $prev i32) (result v128)
    (local $tmp v128)
    ;; v = [a, b, c, d, e, f, g, h]
    ;; Prefix sum of deltas
    ;; Step 1: v += v << 1
    (local.set $tmp (i8x16.shuffle 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 (global.get $zero) (local.get $v)))
    (local.set $v (i16x8.add (local.get $v) (local.get $tmp)))
    ;; Step 2: v += v << 2
    (local.set $tmp (i8x16.shuffle 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 (global.get $zero) (local.get $v)))
    (local.set $v (i16x8.add (local.get $v) (local.get $tmp)))
    ;; Step 3: v += v << 4
    (local.set $tmp (i8x16.shuffle 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 (global.get $zero) (local.get $v)))
    (local.set $v (i16x8.add (local.get $v) (local.get $tmp)))
    ;; Finally add prev to all
    (i16x8.add (local.get $v) (i16x8.splat (local.get $prev)))
  )

  ;; Decode CLC geometry
  (func (export "decode_clc")
    (param $count i32)        ;; number of polygons
    (param $v_total i32)      ;; total vertex count
    (param $i_total i32)      ;; total index count
    
    (local $p i32)            ;; polygon iterator
    (local $meta_ptr i32)
    (local $v_ptr i32)        ;; current vertex in input streams
    (local $i_ptr i32)        ;; current index in input streams
    (local $out_v_ptr i32)    ;; current vertex in output
    (local $out_i_ptr i32)    ;; current index in output

    (local $code i32)
    (local $v_count i32)
    (local $t_count i32)
    (local $code_idx i32)
    (local $base_v i32)

    (local $prev_x i32)
    (local $prev_y i32)
    (local $prev_idx i32)

    (local $k i32)
    (local $v_simd v128)
    (local $vx_simd v128)
    (local $vy_simd v128)
    (local $vi_simd v128)
    (local $v_base v128)
    (local $zx i32)
    (local $zy i32)
    (local $zi i32)
    (local $i_count i32)

    (local.set $meta_ptr (i32.const 0))
    (local.set $v_ptr (i32.const 0))
    (local.set $i_ptr (i32.const 0))
    (local.set $out_v_ptr (i32.const 0))
    (local.set $out_i_ptr (i32.const 0))
    (local.set $p (i32.const 0))

    (loop $poly_loop
      (block $poly_break
        (br_if $poly_break (i32.ge_u (local.get $p) (local.get $count)))

        ;; Read meta
        (local.set $code (i32.load16_u $m_meta (local.get $meta_ptr)))
        (local.set $v_count (i32.load16_u $m_meta (i32.add (local.get $meta_ptr) (i32.const 2))))
        (local.set $t_count (i32.load16_u $m_meta (i32.add (local.get $meta_ptr) (i32.const 4))))
        (local.set $meta_ptr (i32.add (local.get $meta_ptr) (i32.const 6)))

        ;; Lookup code index
        (local.set $code_idx (i32.load8_u $m_palette (local.get $code)))
        (local.set $base_v (local.get $out_v_ptr))

        ;; Decode Vertices
        (local.set $prev_x (i32.const 0))
        (local.set $prev_y (i32.const 0))
        (local.set $k (i32.const 0))
        (loop $v_loop
          (block $v_break
            (br_if $v_break (i32.ge_u (local.get $k) (local.get $v_count)))

            ;; Process 8 vertices at a time if possible
            (if (i32.le_u (i32.add (local.get $k) (i32.const 8)) (local.get $v_count))
              (then
                ;; X Coordinate
                (local.set $v_simd (v128.or 
                  (i16x8.shl (v128.load8x8_u $m_hi_x (i32.add (local.get $v_ptr) (local.get $k))) (i32.const 8))
                  (v128.load8x8_u $m_lo_x (i32.add (local.get $v_ptr) (local.get $k)))
                ))
                (local.set $vx_simd (call $prefix_sum_simd (call $zigzag_simd (local.get $v_simd)) (local.get $prev_x)))
                ;; Update prev_x from last element
                (local.set $prev_x (i32.and (i16x8.extract_lane_u 7 (local.get $vx_simd)) (i32.const 0xFFFF)))

                ;; Y Coordinate
                (local.set $v_simd (v128.or 
                  (i16x8.shl (v128.load8x8_u $m_hi_y (i32.add (local.get $v_ptr) (local.get $k))) (i32.const 8))
                  (v128.load8x8_u $m_lo_y (i32.add (local.get $v_ptr) (local.get $k)))
                ))
                (local.set $vy_simd (call $prefix_sum_simd (call $zigzag_simd (local.get $v_simd)) (local.get $prev_y)))
                (local.set $prev_y (i32.and (i16x8.extract_lane_u 7 (local.get $vy_simd)) (i32.const 0xFFFF)))

                ;; Interleave X,Y and store
                ;; We need to store [X0, Y0, X1, Y1, X2, Y2, X3, Y3]... as i16
                (v128.store $m_out_pos (i32.shl (i32.add (local.get $out_v_ptr) (local.get $k)) (i32.const 2))
                  (i8x16.shuffle 0 1 16 17 2 3 18 19 4 5 20 21 6 7 22 23 (local.get $vx_simd) (local.get $vy_simd)))
                (v128.store $m_out_pos (i32.add (i32.shl (i32.add (local.get $out_v_ptr) (local.get $k)) (i32.const 2)) (i32.const 16))
                  (i8x16.shuffle 8 9 24 25 10 11 26 27 12 13 28 29 14 15 30 31 (local.get $vx_simd) (local.get $vy_simd)))

                ;; Store colors
                (v128.store $m_out_col (i32.add (local.get $out_v_ptr) (local.get $k)) (i8x16.splat (local.get $code_idx)))

                (local.set $k (i32.add (local.get $k) (i32.const 8)))
              )
              (else
                ;; Scalar fallback
                (local.set $zx (i32.or 
                  (i32.shl (i32.load8_u $m_hi_x (i32.add (local.get $v_ptr) (local.get $k))) (i32.const 8))
                  (i32.load8_u $m_lo_x (i32.add (local.get $v_ptr) (local.get $k)))
                ))
                (local.set $prev_x (i32.and (i32.add (local.get $prev_x) 
                  (i32.xor (i32.shr_u (local.get $zx) (i32.const 1)) (i32.sub (i32.const 0) (i32.and (local.get $zx) (i32.const 1)))))
                  (i32.const 0xFFFF)))

                (local.set $zy (i32.or 
                  (i32.shl (i32.load8_u $m_hi_y (i32.add (local.get $v_ptr) (local.get $k))) (i32.const 8))
                  (i32.load8_u $m_lo_y (i32.add (local.get $v_ptr) (local.get $k)))
                ))
                (local.set $prev_y (i32.and (i32.add (local.get $prev_y) 
                  (i32.xor (i32.shr_u (local.get $zy) (i32.const 1)) (i32.sub (i32.const 0) (i32.and (local.get $zy) (i32.const 1)))))
                  (i32.const 0xFFFF)))

                (i32.store16 $m_out_pos (i32.shl (i32.add (local.get $out_v_ptr) (local.get $k)) (i32.const 2)) (local.get $prev_x))
                (i32.store16 $m_out_pos (i32.add (i32.shl (i32.add (local.get $out_v_ptr) (local.get $k)) (i32.const 2)) (i32.const 2)) (local.get $prev_y))
                (i32.store8 $m_out_col (i32.add (local.get $out_v_ptr) (local.get $k)) (local.get $code_idx))

                (local.set $k (i32.add (local.get $k) (i32.const 1)))
              )
            )
            (br $v_loop)
          )
        )
        (local.set $v_ptr (i32.add (local.get $v_ptr) (local.get $v_count)))
        (local.set $out_v_ptr (i32.add (local.get $out_v_ptr) (local.get $v_count)))

        ;; Decode Indices
        (local.set $prev_idx (i32.const 0))
        (local.set $k (i32.const 0))
        (local.set $i_count (i32.mul (local.get $t_count) (i32.const 3)))
        (loop $i_loop
          (block $i_break
            (br_if $i_break (i32.ge_u (local.get $k) (local.get $i_count)))

            (if (i32.le_u (i32.add (local.get $k) (i32.const 8)) (local.get $i_count))
              (then
                (local.set $v_simd (v128.or 
                  (i16x8.shl (v128.load8x8_u $m_hi_i (i32.add (local.get $i_ptr) (local.get $k))) (i32.const 8))
                  (v128.load8x8_u $m_lo_i (i32.add (local.get $i_ptr) (local.get $k)))
                ))
                (local.set $vi_simd (call $prefix_sum_simd (call $zigzag_simd (local.get $v_simd)) (local.get $prev_idx)))
                (local.set $prev_idx (i32.and (i16x8.extract_lane_u 7 (local.get $vi_simd)) (i32.const 0xFFFF)))

                ;; Convert to absolute indices (u16 + base_v) and store as i32
                ;; base_v splat
                (local.set $v_base (i32x4.splat (local.get $base_v)))
                
                ;; Lower 4 elements
                (v128.store $m_out_ebo (i32.shl (i32.add (local.get $out_i_ptr) (local.get $k)) (i32.const 2))
                  (i32x4.add (local.get $v_base) (i32x4.extend_low_i16x8_u (local.get $vi_simd))))
                ;; Upper 4 elements
                (v128.store $m_out_ebo (i32.add (i32.shl (i32.add (local.get $out_i_ptr) (local.get $k)) (i32.const 2)) (i32.const 16))
                  (i32x4.add (local.get $v_base) (i32x4.extend_high_i16x8_u (local.get $vi_simd))))

                (local.set $k (i32.add (local.get $k) (i32.const 8)))
              )
              (else
                (local.set $zi (i32.or 
                  (i32.shl (i32.load8_u $m_hi_i (i32.add (local.get $i_ptr) (local.get $k))) (i32.const 8))
                  (i32.load8_u $m_lo_i (i32.add (local.get $i_ptr) (local.get $k)))
                ))
                (local.set $prev_idx (i32.and (i32.add (local.get $prev_idx) 
                  (i32.xor (i32.shr_u (local.get $zi) (i32.const 1)) (i32.sub (i32.const 0) (i32.and (local.get $zi) (i32.const 1)))))
                  (i32.const 0xFFFF)))

                (i32.store $m_out_ebo (i32.shl (i32.add (local.get $out_i_ptr) (local.get $k)) (i32.const 2)) (i32.add (local.get $base_v) (local.get $prev_idx)))
                (local.set $k (i32.add (local.get $k) (i32.const 1)))
              )
            )
            (br $i_loop)
          )
        )
        (local.set $i_ptr (i32.add (local.get $i_ptr) (local.get $i_count)))
        (local.set $out_i_ptr (i32.add (local.get $out_i_ptr) (local.get $i_count)))

        (local.set $p (i32.add (local.get $p) (i32.const 1)))
        (br $poly_loop)
      )
    )
  )

  ;; Decode Water geometry (24-bit coordinates)
  (func (export "decode_water")
    (param $count i32)        ;; number of polygons
    (param $v_total i32)      ;; total vertex count
    (param $i_total i32)      ;; total index count
    
    (local $p i32) (local $meta_ptr i32)
    (local $v_ptr i32) (local $i_ptr i32)
    (local $out_v_ptr i32) (local $out_i_ptr i32)
    (local $code i32) (local $v_count i32) (local $t_count i32)
    (local $code_idx i32) (local $base_v i32)
    (local $prev_x i32) (local $prev_y i32) (local $prev_idx i32)
    (local $k i32) (local $v_out i32)
    (local $v_lo v128) (local $v_mid v128) (local $v_hi v128)
    (local $idx i32) (local $zx i32) (local $zy i32) (local $zi i32) (local $i_count i32)

    (local.set $meta_ptr (i32.const 0))
    (local.set $v_ptr (i32.const 0))
    (local.set $i_ptr (i32.const 0))
    (local.set $out_v_ptr (i32.const 0))
    (local.set $out_i_ptr (i32.const 0))

    (loop $poly_loop
      (block $poly_break
        (br_if $poly_break (i32.ge_u (local.get $p) (local.get $count)))

        (local.set $code (i32.load16_u $m_meta (local.get $meta_ptr)))
        (local.set $v_count (i32.load16_u $m_meta (i32.add (local.get $meta_ptr) (i32.const 2))))
        (local.set $t_count (i32.load16_u $m_meta (i32.add (local.get $meta_ptr) (i32.const 4))))
        (local.set $meta_ptr (i32.add (local.get $meta_ptr) (i32.const 6)))

        ;; Note: code_idx for water is also looked up from palette, 
        ;; but water codes are also just CLC codes.
        (local.set $code_idx (i32.load8_u $m_palette (local.get $code)))
        (local.set $base_v (local.get $out_v_ptr))

        ;; Vertices (Scalar for 24-bit for simplicity, but optimized)
        (local.set $prev_x (i32.const 0))
        (local.set $prev_y (i32.const 0))
        (local.set $k (i32.const 0))
        (loop $v_loop
          (block $v_break
            (br_if $v_break (i32.ge_u (local.get $k) (local.get $v_count)))
            
            (local.set $idx (i32.add (local.get $v_ptr) (local.get $k)))
            
            (local.set $zx (i32.or (i32.load8_u $m_lo_x (local.get $idx)) 
              (i32.or (i32.shl (i32.load8_u $m_mid_x (local.get $idx)) (i32.const 8))
                      (i32.shl (i32.load8_u $m_hi_x (local.get $idx)) (i32.const 16)))))
            (local.set $prev_x (i32.and (i32.add (local.get $prev_x) (call $zigzag_i32 (local.get $zx))) (global.get $mask24)))

            (local.set $zy (i32.or (i32.load8_u $m_lo_y (local.get $idx)) 
              (i32.or (i32.shl (i32.load8_u $m_mid_y (local.get $idx)) (i32.const 8))
                      (i32.shl (i32.load8_u $m_hi_y (local.get $idx)) (i32.const 16)))))
            (local.set $prev_y (i32.and (i32.add (local.get $prev_y) (call $zigzag_i32 (local.get $zy))) (global.get $mask24)))

            (local.set $v_out (i32.shl (i32.add (local.get $out_v_ptr) (local.get $k)) (i32.const 3)))
            (i32.store $m_out_wpos (local.get $v_out) (local.get $prev_x))
            (i32.store $m_out_wpos (i32.add (local.get $v_out) (i32.const 4)) (local.get $prev_y))
            (i32.store8 $m_out_col (i32.add (local.get $out_v_ptr) (local.get $k)) (local.get $code_idx))

            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $v_loop)
          )
        )
        (local.set $v_ptr (i32.add (local.get $v_ptr) (local.get $v_count)))
        (local.set $out_v_ptr (i32.add (local.get $out_v_ptr) (local.get $v_count)))

        ;; Indices (Scalar fallback same as decode_clc)
        (local.set $prev_idx (i32.const 0))
        (local.set $k (i32.const 0))
        (local.set $i_count (i32.mul (local.get $t_count) (i32.const 3)))
        (loop $i_loop
          (block $i_break
            (br_if $i_break (i32.ge_u (local.get $k) (local.get $i_count)))
            (local.set $zi (i32.or 
              (i32.shl (i32.load8_u $m_hi_i (i32.add (local.get $i_ptr) (local.get $k))) (i32.const 8))
              (i32.load8_u $m_lo_i (i32.add (local.get $i_ptr) (local.get $k)))
            ))
            (local.set $prev_idx (i32.and (i32.add (local.get $prev_idx) (call $zigzag_i32 (local.get $zi))) (i32.const 0xFFFF)))
            (i32.store $m_out_ebo (i32.shl (i32.add (local.get $out_i_ptr) (local.get $k)) (i32.const 2)) (i32.add (local.get $base_v) (local.get $prev_idx)))
            (local.set $k (i32.add (local.get $k) (i32.const 1)))
            (br $i_loop)
          )
        )
        (local.set $i_ptr (i32.add (local.get $i_ptr) (local.get $i_count)))
        (local.set $out_i_ptr (i32.add (local.get $out_i_ptr) (local.get $i_count)))

        (local.set $p (i32.add (local.get $p) (i32.const 1)))
        (br $poly_loop)
      )
    )
  )
)
