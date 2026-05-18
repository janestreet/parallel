(module
   (import "env" "caml_failwith" (func $caml_failwith (param (ref eq))))

   (type $bytes (array (mut i8)))

   (@string $unsupported "Stack pointers are not supported in wasm.")

   (func (export "parallel_stack_pointer_unsupported")
      (param (ref eq)) (result (ref eq))
      (call $caml_failwith (global.get $unsupported))
      (ref.i31 (i32.const 0)))
)
