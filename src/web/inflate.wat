(module
   (import "js" "inflate" (func $inflate_js (param anyref) (result anyref)))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))

   (func (export "inflate") (param $s (ref eq)) (result (ref eq))
      (call $wrap (call $inflate_js (call $unwrap (local.get $s)))))
)
