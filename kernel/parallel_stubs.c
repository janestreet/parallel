#define CAML_INTERNALS

#include <caml/mlvalues.h>

#ifdef CAML_RUNTIME_5

#include <stdbool.h>
#include <pthread.h>
#include <errno.h>

#include <caml/domain.h>
#include <caml/fiber.h>
#include <caml/memory.h>
#include <caml/callback.h>

extern value caml_dynamic_make(value key);
extern value caml_dynamic_get(value key);
extern void caml_dynamic_flush_thread(dynamic_thread_t thread);

static value heartbeat_fls_key;
static value heartbeat_callback;

static void (*existing_domain_tick_hook)(void);

static void parallel_domain_tick_hook() {
  value pointer = caml_dynamic_get(heartbeat_fls_key);
  caml_callback(heartbeat_callback, pointer);

  if (existing_domain_tick_hook) {
    existing_domain_tick_hook();
  }
}

CAMLprim value parallel_create_dynamic(value key) {
  CAMLparam1(key);
  CAMLlocal1(val);

  val = caml_dynamic_make(key);

  CAMLreturn(val);
}

CAMLprim value parallel_unsafe_set_dynamic(value key, value val) {
  CAMLparam2(key, val);

  // Don't need write barrier: key is a global root and val is an immediate
  // Don't need atomics: only this domain can access the current fiber
  Caml_state->current_stack->dyn = key;
  Caml_state->current_stack->val = val;

  // Assure the old binding is not cached
  caml_dynamic_flush_thread(Caml_state->dynamic_bindings);

  CAMLreturn(Val_unit);
}

CAMLprim value parallel_setup_heartbeat(value key, value callback) {
  CAMLparam2(key, callback);

  heartbeat_fls_key = key;
  heartbeat_callback = callback;
  caml_register_generational_global_root(&heartbeat_fls_key);
  caml_register_generational_global_root(&heartbeat_callback);

  CAMLreturn(Val_unit);
}

/* This is separate from `parallel_setup_heartbeat` so we can wait to run it until after
   running [caml_thread_initialize], which also sets [caml_domain_tick_hook].

   Must only be called once per process. */
CAMLprim value parallel_setup_tick_hook(value unit) {
  (void)unit;
  existing_domain_tick_hook = caml_domain_tick_hook;
  caml_domain_tick_hook = &parallel_domain_tick_hook;
  return Val_unit;
}

#else /* CAML_RUNTIME_5 */

CAMLprim value parallel_create_dynamic(__attribute__((unused)) value key) {
  return Val_unit;
}

CAMLprim value parallel_unsafe_set_dynamic(__attribute__((unused)) value key,
                                           __attribute__((unused)) value val) {
  return Val_unit;
}

CAMLprim value parallel_setup_heartbeat(__attribute__((unused)) value key,
                                        __attribute__((unused)) value callback) {
  return Val_unit;
}

CAMLprim value parallel_setup_tick_hook(__attribute__((unused)) value unit) {
  return Val_unit;
}

#endif
