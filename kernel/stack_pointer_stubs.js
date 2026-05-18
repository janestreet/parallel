//Provides: parallel_stack_pointer_unsupported
//Requires: caml_failwith
function parallel_stack_pointer_unsupported(ptr) {
  caml_failwith("Stack pointers are not supported in javascript.")
}
