(module
   (import "js" "inflate" (func $inflate_js (param anyref) (result anyref)))
   (import "js" "inflate_into"
      (func $inflate_into_js (param anyref anyref i32) (result anyref)))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))

   (func (export "inflate") (param $s (ref eq)) (result (ref eq))
      (call $wrap (call $inflate_js (call $unwrap (local.get $s)))))

   (func (export "inflate_into")
      (param $s (ref eq)) (param $t (ref eq)) (param $ofs (ref eq))
      (result (ref eq))
      (call $wrap (call $inflate_into_js (call $unwrap (local.get $s)) (call $unwrap (local.get $t)) (i31.get_u (ref.cast (ref i31) (local.get $ofs))))))
)
