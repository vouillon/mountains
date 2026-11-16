;; Decoding of a layer of a CLC6 tile (land cover or water), as
;; Corine/clc_codec.ml codes it: per feature its rings, arc by arc, then the
;; triangles of each of its components from a walk round them.
;;
;; Output, as the renderer takes it: per vertex its two coordinates (u16 for
;; the land cover, u32 for the water) and its palette index; per triangle
;; three u32 vertex numbers, counted over the whole layer.
(module
  (import "env" "memory" (memory 1))

  ;; Cursors of the two streams of varints, which are read a few times per
  ;; ring
  (global $tp (mut i32) (i32.const 0))    ;; topology
  (global $ap (mut i32) (i32.const 0))    ;; arcs
  ;; The layer being decoded, as [start_layer] was given it, and how far
  ;; [decode_features] has gone in it: features, vertices, points of
  ;; coordinates, masks, indices, arcs stored, first point of the last ring
  (global $g_count (mut i32) (i32.const 0))
  (global $g_wide (mut i32) (i32.const 0))
  (global $g_codes (mut i32) (i32.const 0))
  (global $g_hx (mut i32) (i32.const 0))
  (global $g_mx (mut i32) (i32.const 0))
  (global $g_lx (mut i32) (i32.const 0))
  (global $g_hy (mut i32) (i32.const 0))
  (global $g_my (mut i32) (i32.const 0))
  (global $g_ly (mut i32) (i32.const 0))
  (global $g_conn (mut i32) (i32.const 0))
  (global $g_pos (mut i32) (i32.const 0))
  (global $g_col (mut i32) (i32.const 0))
  (global $g_idx (mut i32) (i32.const 0))
  (global $g_palette (mut i32) (i32.const 0))
  (global $g_arcs (mut i32) (i32.const 0))
  (global $g_rings (mut i32) (i32.const 0))
  (global $g_walk (mut i32) (i32.const 0))
  (global $g_stack (mut i32) (i32.const 0))
  (global $g_f (mut i32) (i32.const 0))
  (global $g_nv (mut i32) (i32.const 0))
  (global $g_cp (mut i32) (i32.const 0))
  (global $g_mp (mut i32) (i32.const 0))
  (global $g_ni (mut i32) (i32.const 0))
  (global $g_nreg (mut i32) (i32.const 0))
  (global $g_cx (mut i32) (i32.const 0))
  (global $g_cy (mut i32) (i32.const 0))

  (func $topo (result i32)
    (local $v i32) (local $s i32) (local $c i32)
    (loop $l
      (local.set $c (i32.load8_u (global.get $tp)))
      (global.set $tp (i32.add (global.get $tp) (i32.const 1)))
      (local.set $v (i32.or (local.get $v)
        (i32.shl (i32.and (local.get $c) (i32.const 0x7F)) (local.get $s))))
      (local.set $s (i32.add (local.get $s) (i32.const 7)))
      (br_if $l (i32.ge_u (local.get $c) (i32.const 0x80))))
    (local.get $v))

  (func $op (result i32)
    (local $v i32) (local $s i32) (local $c i32)
    (loop $l
      (local.set $c (i32.load8_u (global.get $ap)))
      (global.set $ap (i32.add (global.get $ap) (i32.const 1)))
      (local.set $v (i32.or (local.get $v)
        (i32.shl (i32.and (local.get $c) (i32.const 0x7F)) (local.get $s))))
      (local.set $s (i32.add (local.get $s) (i32.const 7)))
      (br_if $l (i32.ge_u (local.get $c) (i32.const 0x80))))
    (local.get $v))

  (func $unzigzag (param $z i32) (result i32)
    (i32.xor (i32.shr_u (local.get $z) (i32.const 1))
      (i32.sub (i32.const 0) (i32.and (local.get $z) (i32.const 1)))))

  ;; The coordinates of vertex [v]
  (func $x (param $pos i32) (param $wide i32) (param $v i32) (result i32)
    (if (result i32) (local.get $wide)
      (then (i32.load (i32.add (local.get $pos) (i32.shl (local.get $v) (i32.const 3)))))
      (else (i32.load16_u (i32.add (local.get $pos) (i32.shl (local.get $v) (i32.const 2)))))))

  (func $y (param $pos i32) (param $wide i32) (param $v i32) (result i32)
    (if (result i32) (local.get $wide)
      (then (i32.load offset=4 (i32.add (local.get $pos) (i32.shl (local.get $v) (i32.const 3)))))
      (else (i32.load16_u offset=2 (i32.add (local.get $pos) (i32.shl (local.get $v) (i32.const 2)))))))

  ;; Copy vertex [src] to [dst]
  (func $copy (param $pos i32) (param $wide i32) (param $src i32) (param $dst i32)
    (if (local.get $wide)
      (then
        (i64.store (i32.add (local.get $pos) (i32.shl (local.get $dst) (i32.const 3)))
          (i64.load (i32.add (local.get $pos) (i32.shl (local.get $src) (i32.const 3))))))
      (else
        (i32.store (i32.add (local.get $pos) (i32.shl (local.get $dst) (i32.const 2)))
          (i32.load (i32.add (local.get $pos) (i32.shl (local.get $src) (i32.const 2))))))))

  ;; Copy [n] vertices, from [src] on by steps of [step] (1 or -1), to [dst]
  ;; on
  (func $copy_run (param $pos i32) (param $wide i32) (param $src i32) (param $step i32)
    (param $n i32) (param $dst i32)
    (local $s i32) (local $d i32) (local $e i32)
    (if (local.get $wide)
      (then
        (local.set $s (i32.add (local.get $pos) (i32.shl (local.get $src) (i32.const 3))))
        (local.set $d (i32.add (local.get $pos) (i32.shl (local.get $dst) (i32.const 3))))
        (local.set $e (i32.add (local.get $d) (i32.shl (local.get $n) (i32.const 3))))
        (local.set $step (i32.shl (local.get $step) (i32.const 3)))
        (block $done
          (loop $l
            (br_if $done (i32.ge_u (local.get $d) (local.get $e)))
            (i64.store (local.get $d) (i64.load (local.get $s)))
            (local.set $s (i32.add (local.get $s) (local.get $step)))
            (local.set $d (i32.add (local.get $d) (i32.const 8)))
            (br $l))))
      (else
        (local.set $s (i32.add (local.get $pos) (i32.shl (local.get $src) (i32.const 2))))
        (local.set $d (i32.add (local.get $pos) (i32.shl (local.get $dst) (i32.const 2))))
        (local.set $e (i32.add (local.get $d) (i32.shl (local.get $n) (i32.const 2))))
        (local.set $step (i32.shl (local.get $step) (i32.const 2)))
        (block $done
          (loop $l
            (br_if $done (i32.ge_u (local.get $d) (local.get $e)))
            (i32.store (local.get $d) (i32.load (local.get $s)))
            (local.set $s (i32.add (local.get $s) (local.get $step)))
            (local.set $d (i32.add (local.get $d) (i32.const 4)))
            (br $l))))))

  ;; Start on a layer of [count] features. The coordinate streams are given
  ;; most significant byte first ([mx], [my] unused for the land cover).
  ;; [arcs] takes 12 bytes per arc stored; [rings] 8 bytes, [walk] 4 bytes
  ;; and [stack] 4 bytes per position of the longest walk.
  (func (export "start_layer")
    (param $count i32) (param $wide i32)
    (param $codes i32) (param $topo_ptr i32) (param $ops_ptr i32)
    (param $hx i32) (param $mx i32) (param $lx i32)
    (param $hy i32) (param $my i32) (param $ly i32)
    (param $conn i32)
    (param $pos i32) (param $col i32) (param $idx i32) (param $palette i32)
    (param $arcs i32) (param $rings i32) (param $walk i32) (param $stack i32)
    (global.set $tp (local.get $topo_ptr))
    (global.set $ap (local.get $ops_ptr))
    (global.set $g_count (local.get $count))
    (global.set $g_wide (local.get $wide))
    (global.set $g_codes (local.get $codes))
    (global.set $g_hx (local.get $hx))
    (global.set $g_mx (local.get $mx))
    (global.set $g_lx (local.get $lx))
    (global.set $g_hy (local.get $hy))
    (global.set $g_my (local.get $my))
    (global.set $g_ly (local.get $ly))
    (global.set $g_conn (local.get $conn))
    (global.set $g_pos (local.get $pos))
    (global.set $g_col (local.get $col))
    (global.set $g_idx (local.get $idx))
    (global.set $g_palette (local.get $palette))
    (global.set $g_arcs (local.get $arcs))
    (global.set $g_rings (local.get $rings))
    (global.set $g_walk (local.get $walk))
    (global.set $g_stack (local.get $stack))
    (global.set $g_f (i32.const 0))
    (global.set $g_nv (i32.const 0))
    (global.set $g_cp (i32.const 0))
    (global.set $g_mp (i32.const 0))
    (global.set $g_ni (i32.const 0))
    (global.set $g_nreg (i32.const 0))
    (global.set $g_cx (i32.const 0))
    (global.set $g_cy (i32.const 0))
  )

  (func (export "vertices") (result i32) (global.get $g_nv))
  (func (export "indices") (result i32) (global.get $g_ni))

  ;; Decode the next [n] features at most; returns how many are left. A
  ;; layer goes in several calls so that the engine can swap in optimized
  ;; code for the calls after the first ones: it cannot within a call.
  (func (export "decode_features") (param $n i32) (result i32)
    (local $count i32)
    (local $wide i32)
    (local $codes i32)
    (local $hx i32)
    (local $mx i32)
    (local $lx i32)
    (local $hy i32)
    (local $my i32)
    (local $ly i32)
    (local $conn i32)
    (local $pos i32)
    (local $col i32)
    (local $idx i32)
    (local $palette i32)
    (local $arcs i32)
    (local $rings i32)
    (local $walk i32)
    (local $stack i32)
    (local $end i32)
    (local $color i32) (local $vbase i32)
    (local $ncomp i32) (local $c i32) (local $nr i32) (local $r i32)
    (local $rb i32) (local $narcs i32) (local $a i32) (local $tok i32)
    (local $closing i32) (local $k i32) (local $s i32) (local $t i32)
    (local $rec i32) (local $p0 i32) (local $pk i32)
    (local $px i32) (local $py i32) (local $z i32)
    (local $m i32) (local $nruns i32) (local $q i32) (local $len i32) (local $base i32)
    (local $sp i32) (local $slot i32) (local $msk i32) (local $fr i32) (local $at i32)
    (local $f i32)
    (local $nv i32)
    (local $cp i32)
    (local $mp i32)
    (local $ni i32)
    (local $nreg i32)
    (local $cx i32)
    (local $cy i32)
    (local.set $count (global.get $g_count))
    (local.set $wide (global.get $g_wide))
    (local.set $codes (global.get $g_codes))
    (local.set $hx (global.get $g_hx))
    (local.set $mx (global.get $g_mx))
    (local.set $lx (global.get $g_lx))
    (local.set $hy (global.get $g_hy))
    (local.set $my (global.get $g_my))
    (local.set $ly (global.get $g_ly))
    (local.set $conn (global.get $g_conn))
    (local.set $pos (global.get $g_pos))
    (local.set $col (global.get $g_col))
    (local.set $idx (global.get $g_idx))
    (local.set $palette (global.get $g_palette))
    (local.set $arcs (global.get $g_arcs))
    (local.set $rings (global.get $g_rings))
    (local.set $walk (global.get $g_walk))
    (local.set $stack (global.get $g_stack))
    (local.set $f (global.get $g_f))
    (local.set $nv (global.get $g_nv))
    (local.set $cp (global.get $g_cp))
    (local.set $mp (global.get $g_mp))
    (local.set $ni (global.get $g_ni))
    (local.set $nreg (global.get $g_nreg))
    (local.set $cx (global.get $g_cx))
    (local.set $cy (global.get $g_cy))
    (local.set $end (select (local.get $count) (i32.add (local.get $f) (local.get $n))
      (i32.gt_u (i32.add (local.get $f) (local.get $n)) (local.get $count))))

    (block $features_done
      (loop $features
        (br_if $features_done (i32.ge_u (local.get $f) (local.get $end)))
        (local.set $color (i32.load8_u (i32.add (local.get $palette)
          (i32.load16_u (i32.add (local.get $codes) (i32.shl (local.get $f) (i32.const 1)))))))
        (local.set $vbase (local.get $nv))
        (local.set $ncomp (call $topo))
        (local.set $c (i32.const 0))
        (block $comps_done
          (loop $comps
            (br_if $comps_done (i32.ge_u (local.get $c) (local.get $ncomp)))
            (local.set $nr (call $topo))

            ;; The component's rings
            (local.set $r (i32.const 0))
            (block $rings_done
              (loop $rings_loop
                (br_if $rings_done (i32.ge_u (local.get $r) (local.get $nr)))
                (local.set $rb (local.get $nv))
                (local.set $narcs (call $op))
                (local.set $a (i32.const 0))
                (block $arcs_done
                  (loop $arcs_loop
                    (br_if $arcs_done (i32.ge_u (local.get $a) (local.get $narcs)))
                    (local.set $tok (call $op))
                    (local.set $closing (i32.eq (local.get $a) (i32.sub (local.get $narcs) (i32.const 1))))
                    (if (i32.eqz (i32.and (local.get $tok) (i32.const 1)))
                      (then
                        ;; Points of its own, each a delta from the one before:
                        ;; from the end of the previous arc, or, for the
                        ;; first of a ring, from the previous ring's first
                        ;; point. The last arc of a ring does not repeat the
                        ;; ring's first point.
                        (local.set $k (i32.shr_u (local.get $tok) (i32.const 2)))
                        (local.set $t (i32.sub (local.get $k) (local.get $closing)))
                        (if (i32.eqz (local.get $a))
                          (then
                            (local.set $s (local.get $nv))
                            (local.set $t (i32.add (local.get $t) (i32.const 1)))
                            (local.set $px (local.get $cx))
                            (local.set $py (local.get $cy)))
                          (else
                            (local.set $s (i32.sub (local.get $nv) (i32.const 1)))
                            (local.set $px (call $x (local.get $pos) (local.get $wide) (local.get $s)))
                            (local.set $py (call $y (local.get $pos) (local.get $wide) (local.get $s)))))
                        (if (local.get $wide)
                          (then
                            (block $wide_done
                              (loop $wide_pts
                                (br_if $wide_done (i32.eqz (local.get $t)))
                                (local.set $z (i32.or (i32.or
                                  (i32.shl (i32.load8_u (i32.add (local.get $hx) (local.get $cp))) (i32.const 16))
                                  (i32.shl (i32.load8_u (i32.add (local.get $mx) (local.get $cp))) (i32.const 8)))
                                  (i32.load8_u (i32.add (local.get $lx) (local.get $cp)))))
                                (local.set $px (i32.and (i32.add (local.get $px) (call $unzigzag (local.get $z)))
                                  (i32.const 0xFFFFFF)))
                                (local.set $z (i32.or (i32.or
                                  (i32.shl (i32.load8_u (i32.add (local.get $hy) (local.get $cp))) (i32.const 16))
                                  (i32.shl (i32.load8_u (i32.add (local.get $my) (local.get $cp))) (i32.const 8)))
                                  (i32.load8_u (i32.add (local.get $ly) (local.get $cp)))))
                                (local.set $py (i32.and (i32.add (local.get $py) (call $unzigzag (local.get $z)))
                                  (i32.const 0xFFFFFF)))
                                (local.set $rec (i32.add (local.get $pos) (i32.shl (local.get $nv) (i32.const 3))))
                                (i32.store (local.get $rec) (local.get $px))
                                (i32.store offset=4 (local.get $rec) (local.get $py))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1)))
                                (local.set $cp (i32.add (local.get $cp) (i32.const 1)))
                                (local.set $t (i32.sub (local.get $t) (i32.const 1)))
                                (br $wide_pts))))
                          (else
                            ;; 16 bits: the sums need no masking, the stores
                            ;; keep the low half
                            (block $pts_done
                              (loop $pts
                                (br_if $pts_done (i32.eqz (local.get $t)))
                                (local.set $px (i32.add (local.get $px) (call $unzigzag (i32.or
                                  (i32.shl (i32.load8_u (i32.add (local.get $hx) (local.get $cp))) (i32.const 8))
                                  (i32.load8_u (i32.add (local.get $lx) (local.get $cp)))))))
                                (local.set $py (i32.add (local.get $py) (call $unzigzag (i32.or
                                  (i32.shl (i32.load8_u (i32.add (local.get $hy) (local.get $cp))) (i32.const 8))
                                  (i32.load8_u (i32.add (local.get $ly) (local.get $cp)))))))
                                (local.set $rec (i32.add (local.get $pos) (i32.shl (local.get $nv) (i32.const 2))))
                                (i32.store16 (local.get $rec) (local.get $px))
                                (i32.store16 offset=2 (local.get $rec) (local.get $py))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1)))
                                (local.set $cp (i32.add (local.get $cp) (i32.const 1)))
                                (local.set $t (i32.sub (local.get $t) (i32.const 1)))
                                (br $pts)))))
                        ;; Stored for a feature further on: where its first
                        ;; point is (the others follow it, but for the last
                        ;; one of a ring's last arc), where its last one is,
                        ;; its length
                        (if (i32.and (local.get $tok) (i32.const 2))
                          (then
                            (local.set $rec (i32.add (local.get $arcs) (i32.mul (local.get $nreg) (i32.const 12))))
                            (i32.store (local.get $rec) (local.get $s))
                            (i32.store offset=4 (local.get $rec)
                              (select (local.get $rb) (i32.add (local.get $s) (local.get $k)) (local.get $closing)))
                            (i32.store offset=8 (local.get $rec) (local.get $k))
                            (local.set $nreg (i32.add (local.get $nreg) (i32.const 1))))))
                      (else
                        ;; An arc stored before, forwards or backwards
                        (local.set $rec (i32.add (local.get $arcs)
                          (i32.mul (i32.shr_u (local.get $tok) (i32.const 2)) (i32.const 12))))
                        (local.set $p0 (i32.load (local.get $rec)))
                        (local.set $pk (i32.load offset=4 (local.get $rec)))
                        (local.set $k (i32.load offset=8 (local.get $rec)))
                        (if (i32.and (local.get $tok) (i32.const 2))
                          (then
                            (if (i32.eqz (local.get $a))
                              (then
                                (call $copy (local.get $pos) (local.get $wide) (local.get $pk) (local.get $nv))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1)))))
                            (call $copy_run (local.get $pos) (local.get $wide)
                              (i32.add (local.get $p0) (i32.sub (local.get $k) (i32.const 1))) (i32.const -1)
                              (i32.sub (local.get $k) (i32.const 1)) (local.get $nv))
                            (local.set $nv (i32.add (local.get $nv) (i32.sub (local.get $k) (i32.const 1))))
                            (if (i32.eqz (local.get $closing))
                              (then
                                (call $copy (local.get $pos) (local.get $wide) (local.get $p0) (local.get $nv))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1))))))
                          (else
                            (if (i32.eqz (local.get $a))
                              (then
                                (call $copy (local.get $pos) (local.get $wide) (local.get $p0) (local.get $nv))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1)))))
                            (call $copy_run (local.get $pos) (local.get $wide)
                              (i32.add (local.get $p0) (i32.const 1)) (i32.const 1)
                              (i32.sub (local.get $k) (i32.const 1)) (local.get $nv))
                            (local.set $nv (i32.add (local.get $nv) (i32.sub (local.get $k) (i32.const 1))))
                            (if (i32.eqz (local.get $closing))
                              (then
                                (call $copy (local.get $pos) (local.get $wide) (local.get $pk) (local.get $nv))
                                (local.set $nv (i32.add (local.get $nv) (i32.const 1)))))))))
                    (local.set $a (i32.add (local.get $a) (i32.const 1)))
                    (br $arcs_loop)))
                (local.set $cx (call $x (local.get $pos) (local.get $wide) (local.get $rb)))
                (local.set $cy (call $y (local.get $pos) (local.get $wide) (local.get $rb)))
                (local.set $rec (i32.add (local.get $rings) (i32.shl (local.get $r) (i32.const 3))))
                (i32.store (local.get $rec) (local.get $rb))
                (i32.store offset=4 (local.get $rec) (i32.sub (local.get $nv) (local.get $rb)))
                (local.set $r (i32.add (local.get $r) (i32.const 1)))
                (br $rings_loop)))

            ;; The walk round the component: its ring, or runs of its rings
            (local.set $m (i32.const 0))
            (if (i32.eq (local.get $nr) (i32.const 1))
              (then
                (local.set $base (i32.load (local.get $rings)))
                (local.set $len (i32.load offset=4 (local.get $rings)))
                (block $one_done
                  (loop $one
                    (br_if $one_done (i32.ge_u (local.get $m) (local.get $len)))
                    (i32.store (i32.add (local.get $walk) (i32.shl (local.get $m) (i32.const 2)))
                      (i32.add (local.get $base) (local.get $m)))
                    (local.set $m (i32.add (local.get $m) (i32.const 1)))
                    (br $one))))
              (else
                (local.set $nruns (call $topo))
                (block $runs_done
                  (loop $runs
                    (br_if $runs_done (i32.eqz (local.get $nruns)))
                    (local.set $rec (i32.add (local.get $rings) (i32.shl (call $topo) (i32.const 3))))
                    (local.set $base (i32.load (local.get $rec)))
                    (local.set $len (i32.load offset=4 (local.get $rec)))
                    (local.set $q (call $topo))
                    (local.set $t (call $topo))
                    (block $run_done
                      (loop $run
                        (br_if $run_done (i32.eqz (local.get $t)))
                        (i32.store (i32.add (local.get $walk) (i32.shl (local.get $m) (i32.const 2)))
                          (i32.add (local.get $base) (local.get $q)))
                        (local.set $m (i32.add (local.get $m) (i32.const 1)))
                        (local.set $q (i32.add (local.get $q) (i32.const 1)))
                        (if (i32.eq (local.get $q) (local.get $len)) (then (local.set $q (i32.const 0))))
                        (local.set $t (i32.sub (local.get $t) (i32.const 1)))
                        (br $run)))
                    (local.set $nruns (i32.sub (local.get $nruns) (i32.const 1)))
                    (br $runs)))))

            ;; The triangles: the tree of the walk in preorder, two bits a
            ;; triangle (has a left child, has a right one). A position [at]
            ;; goes round the walk: a triangle's first vertex is where it
            ;; stands when the triangle comes, its second where it stands
            ;; once the left subtree is done -- one step on if there is none
            ;; -- and its third likewise after the right subtree. A triangle
            ;; waiting for a subtree is on the stack, as the address of its
            ;; slot in [idx] plus 0 (waiting for the left subtree, no right
            ;; one), 1 (the left one, a right one to come) or 2 (the right
            ;; one). The masks are about as random as two bits can be, so
            ;; this goes with as few branches as it can: a slot is written as
            ;; if it were known, and again once it is, and a triangle is
            ;; pushed by moving the stack pointer by 4 or by 0.
            (local.set $at (i32.const 0))
            (local.set $sp (local.get $stack))
            (block $tree_done
              (loop $visit
                (local.set $msk (i32.and
                  (i32.shr_u
                    (i32.load8_u (i32.add (local.get $conn) (i32.shr_u (local.get $mp) (i32.const 2))))
                    (i32.shl (i32.and (local.get $mp) (i32.const 3)) (i32.const 1)))
                  (i32.const 3)))
                (local.set $mp (i32.add (local.get $mp) (i32.const 1)))
                (local.set $slot (i32.add (local.get $idx) (i32.shl (local.get $ni) (i32.const 2))))
                (local.set $ni (i32.add (local.get $ni) (i32.const 3)))
                (i32.store (local.get $slot)
                  (i32.load (i32.add (local.get $walk) (i32.shl (local.get $at) (i32.const 2)))))
                ;; No left child: one step for it
                (local.set $at (i32.add (local.get $at)
                  (i32.eqz (i32.and (local.get $msk) (i32.const 1)))))
                (i32.store offset=4 (local.get $slot)
                  (i32.load (i32.add (local.get $walk) (i32.shl (local.get $at) (i32.const 2)))))
                ;; No child at all: one more for the right one
                (local.set $at (i32.add (local.get $at) (i32.eqz (local.get $msk))))
                (i32.store offset=8 (local.get $slot)
                  (i32.load (i32.add (local.get $walk) (i32.shl (local.get $at) (i32.const 2)))))
                ;; With a left child, wait for it (0 or 1); with a right one
                ;; only, wait for that (2)
                (i32.store (local.get $sp)
                  (i32.or (local.get $slot)
                    (select (i32.shr_u (local.get $msk) (i32.const 1)) (i32.const 2)
                      (i32.and (local.get $msk) (i32.const 1)))))
                (local.set $sp (i32.add (local.get $sp)
                  (i32.shl (i32.ne (local.get $msk) (i32.const 0)) (i32.const 2))))
                (br_if $visit (local.get $msk))
                ;; A subtree done: on to the triangles waiting for it. The
                ;; position is the second vertex of one waiting for its left
                ;; subtree (one more step to its third if it has no right
                ;; child), the third of one waiting for its right subtree.
                (loop $unwind
                  (br_if $tree_done (i32.eq (local.get $sp) (local.get $stack)))
                  (local.set $fr (i32.load offset=0 (i32.sub (local.get $sp) (i32.const 4))))
                  (local.set $slot (i32.and (local.get $fr) (i32.const -4)))
                  (local.set $fr (i32.and (local.get $fr) (i32.const 3)))
                  (i32.store offset=4
                    (i32.add (local.get $slot)
                      (i32.shl (i32.eq (local.get $fr) (i32.const 2)) (i32.const 2)))
                    (i32.load (i32.add (local.get $walk) (i32.shl (local.get $at) (i32.const 2)))))
                  (local.set $at (i32.add (local.get $at) (i32.eqz (local.get $fr))))
                  (i32.store offset=8 (local.get $slot)
                    (i32.load (i32.add (local.get $walk) (i32.shl (local.get $at) (i32.const 2)))))
                  ;; Waiting for a right subtree to come: it stays, for that
                  (i32.store offset=0 (i32.sub (local.get $sp) (i32.const 4))
                    (i32.or (local.get $slot) (i32.const 2)))
                  (local.set $sp (i32.sub (local.get $sp)
                    (i32.shl (i32.ne (local.get $fr) (i32.const 1)) (i32.const 2))))
                  (br_if $visit (i32.eq (local.get $fr) (i32.const 1)))
                  (br $unwind))))

            (local.set $c (i32.add (local.get $c) (i32.const 1)))
            (br $comps)))
        (memory.fill (i32.add (local.get $col) (local.get $vbase)) (local.get $color)
          (i32.sub (local.get $nv) (local.get $vbase)))
        (local.set $f (i32.add (local.get $f) (i32.const 1)))
        (br $features)))
    (global.set $g_f (local.get $f))
    (global.set $g_nv (local.get $nv))
    (global.set $g_cp (local.get $cp))
    (global.set $g_mp (local.get $mp))
    (global.set $g_ni (local.get $ni))
    (global.set $g_nreg (local.get $nreg))
    (global.set $g_cx (local.get $cx))
    (global.set $g_cy (local.get $cy))
    (i32.sub (local.get $count) (local.get $f)))
)
