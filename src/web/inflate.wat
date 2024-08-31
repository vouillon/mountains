(module
   (import "js" "inflate" (func $inflate (param anyref) (result anyref)))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))

   (func (export "inflate") (param $s (ref eq)) (result (ref eq))
      (call $wrap (call $inflate (call $unwrap (local.get $s)))))
)
