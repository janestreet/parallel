#include <caml/alloc.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

// These symbols are also defined in ocaml_intrinsics. If an executable depends on both
// parallel_kernel and ocaml_intrinsics, they will conflict. Therefore, we've made these
// symbols weak: they will be overridden by the versions in ocaml_intrinsics if necessary.

CAMLweakdef intnat caml_native_pointer_of_value(__attribute__((unused)) value _) {
  caml_fatal_error(
      "caml_native_pointer_of_value: this function is a builtin and should not be called "
      "from native code");
}

CAMLweakdef value caml_native_pointer_to_value(__attribute__((unused)) intnat _) {
  caml_fatal_error(
      "caml_native_pointer_to_value: this function is a builtin and should not be called "
      "from native code");
}

CAMLweakdef value caml_ext_pointer_as_native_pointer(__attribute__((unused)) intnat _) {
  caml_fatal_error("caml_ext_pointer_as_native_pointer: this function is a builtin and "
                   "should not be called from native code");
}

CAMLweakdef CAMLprim value caml_native_pointer_of_value_bytecode(__attribute__((unused))
                                                                 value _) {
  caml_fatal_error(
      "caml_native_pointer_of_value_bytecode: the only bytecode backend we support is "
      "JavaScript, which should not call this function");
}

CAMLweakdef CAMLprim value caml_native_pointer_to_value_bytecode(__attribute__((unused))
                                                                 value _) {
  caml_fatal_error(
      "caml_native_pointer_to_value_bytecode: the only bytecode backend we support is "
      "JavaScript, which should not call this function");
}

CAMLweakdef CAMLprim value
caml_ext_pointer_as_native_pointer_bytecode(__attribute__((unused)) value _) {
  caml_fatal_error(
      "caml_ext_pointer_as_native_pointer_bytecode: the only bytecode "
      "backend we support is JavaScript, which should not call this function");
}
