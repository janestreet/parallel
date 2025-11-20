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

/* NB: this value must be bitwise disjoint from ST_INTERRUPT_FLAG, which is
   currently 1 */
#define HB_INTERRUPT_FLAG ((uintnat)(1 << 1))

typedef struct {
  pthread_mutex_t mut;
  pthread_cond_t cond;
  uint64_t count;
} heartbeat_refcount_t;
static heartbeat_refcount_t heartbeat_refcount = {PTHREAD_MUTEX_INITIALIZER,
                                                  PTHREAD_COND_INITIALIZER, 0};

static value heartbeat_fls_key;
static value heartbeat_callback;
static uintnat heartbeat_interval_us;

static void (*existing_domain_external_interrupt_hook)(void);

static void parallel_domain_external_interrupt_hook() {
  uintnat mask = ~HB_INTERRUPT_FLAG;
  atomic_uintnat *request = &Caml_state->requested_external_interrupt;

  if (atomic_fetch_and_explicit(request, mask, memory_order_seq_cst) &
      HB_INTERRUPT_FLAG) {
    value pointer = caml_dynamic_get(heartbeat_fls_key);
    caml_callback(heartbeat_callback, pointer);
  }

  if (existing_domain_external_interrupt_hook) {
    existing_domain_external_interrupt_hook();
  }
}

static void *parallel_heartbeat_thread(__attribute__((unused)) void *param) {

  struct timespec interval = {0};
  interval.tv_nsec = heartbeat_interval_us * 1000;

  while (true) {

    pthread_mutex_lock(&heartbeat_refcount.mut);
    while (heartbeat_refcount.count == 0) {
      pthread_cond_wait(&heartbeat_refcount.cond, &heartbeat_refcount.mut);
    }
    pthread_mutex_unlock(&heartbeat_refcount.mut);

    struct timespec remain = {0};

#ifdef __APPLE__
    int err = nanosleep(&interval, &remain);
    while (err == EINTR) {
      err = nanosleep(&remain, &remain);
    }
#else
    int err = clock_nanosleep(CLOCK_MONOTONIC, 0, &interval, &remain);
    while (err == EINTR) {
      err = clock_nanosleep(CLOCK_MONOTONIC, 0, &remain, &remain);
    }
#endif

    if (err) {
      caml_fatal_error("Heartbeat thread failed to sleep: %d\n", err);
    }

    // We may request external interrupts without holding a domain lock
    caml_external_interrupt_all_signal_safe(HB_INTERRUPT_FLAG);
  }

  return NULL;
}

static void parallel_heartbeat_incref() {
  pthread_mutex_lock(&heartbeat_refcount.mut);
  if (heartbeat_refcount.count++ == 0) {
    pthread_cond_signal(&heartbeat_refcount.cond);
  }
  pthread_mutex_unlock(&heartbeat_refcount.mut);
}

static void parallel_heartbeat_decref() {
  pthread_mutex_lock(&heartbeat_refcount.mut);
  heartbeat_refcount.count--;
  pthread_mutex_unlock(&heartbeat_refcount.mut);
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

CAMLprim value parallel_setup_heartbeat(value interval_us, value key, value callback) {
  CAMLparam3(interval_us, key, callback);

  heartbeat_interval_us = Long_val(interval_us);
  heartbeat_fls_key = key;
  heartbeat_callback = callback;
  caml_register_generational_global_root(&heartbeat_fls_key);
  caml_register_generational_global_root(&heartbeat_callback);

  CAMLreturn(Val_unit);
}

CAMLprim value parallel_acquire_heartbeat(__attribute__((unused)) value unit) {

  // Spawn the thread here rather than [parallel_setup_heartbeat] to assure we
  // install the hook after running [caml_thread_initialize].
  static atomic_uintnat heartbeat_running = 0;
  uintnat running = 0;
  /* relaxed here is ok since our only concern is that [running] is set once */
  if (atomic_load_explicit(&heartbeat_running, memory_order_relaxed) == 0 &&
      atomic_compare_exchange_strong_explicit(
          &heartbeat_running, &running, 1, memory_order_relaxed, memory_order_relaxed)) {

    existing_domain_external_interrupt_hook = caml_domain_external_interrupt_hook;
    caml_domain_external_interrupt_hook = &parallel_domain_external_interrupt_hook;

    // Heartbeat thread should not handle signals
    sigset_t mask, old_mask;
    sigfillset(&mask);
    pthread_sigmask(SIG_BLOCK, &mask, &old_mask);

    pthread_t thread;
    int err = pthread_create(&thread, NULL, parallel_heartbeat_thread, NULL);
    if (err) {
      caml_fatal_error("Failed to create heartbeat thread: %d\n", err);
    }

    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
  }

  parallel_heartbeat_incref();
  return Val_unit;
}

CAMLprim value parallel_release_heartbeat(__attribute__((unused)) value unit) {
  parallel_heartbeat_decref();
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

CAMLprim value parallel_acquire_heartbeat(__attribute__((unused)) value unit) {
  return Val_unit;
}

CAMLprim value parallel_release_heartbeat(__attribute__((unused)) value unit) {
  return Val_unit;
}

CAMLprim value parallel_setup_heartbeat(__attribute__((unused)) value interval_us,
                                        __attribute__((unused)) value key,
                                        __attribute__((unused)) value callback) {
  return Val_unit;
}

#endif
