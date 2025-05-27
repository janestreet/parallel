#include <caml/mlvalues.h>

#ifdef CAML_RUNTIME_5

#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>

struct Custom_Atomic {
  header_t header;
  atomic_value count;
};

static struct Custom_Atomic heartbeat_counter = {Caml_out_of_heap_header(1, No_scan_tag),
                                                 Val_long(-1)};

CAMLprim value parallel_heartbeat_counter(__attribute__((unused)) value unit) {
  return (value)&heartbeat_counter.count;
}

static void *heartbeat_thread(void *interval_us) {

  // We do not hold a domain lock, so we must not interact with the OCaml runtime.
  // However, we cheat and update [heartbeat_counter] because we know it is not
  // GC-managed.

  struct timespec interval = {0};
  interval.tv_nsec = (long)interval_us * 1000;

  while (true) {
    struct timespec remain = {0};
    int err = clock_nanosleep(CLOCK_MONOTONIC, 0, &interval, &remain);
    while (err == EINTR) {
      err = clock_nanosleep(CLOCK_MONOTONIC, 0, &remain, &remain);
    }

    if (err) {
      caml_fatal_error("Heartbeat thread failed to sleep: %d\n", err);
    }

    // Adding two increments the tagged count.
    atomic_fetch_add(&heartbeat_counter.count, 2);
    atomic_thread_fence(memory_order_release);
  }

  return NULL;
}

CAMLprim value parallel_start_heartbeating(value interval_us) {

  value count = Val_long(-1);
  int success =
      atomic_compare_exchange_strong(&heartbeat_counter.count, &count, Val_long(0));
  atomic_thread_fence(memory_order_release);

  if (success) {
    // Write barrier not required because count is immediate.
    pthread_t thread;
    long interval = Long_val(interval_us);
    int err = pthread_create(&thread, NULL, heartbeat_thread, (void *)interval);
    if (err) {
      caml_fatal_error("Failed to create heartbeat thread: %d\n", err);
    }
  }

  return Val_unit;
}

#else /* #ifdef CAML_RUNTIME_5 */

#include <caml/alloc.h>

CAMLprim value parallel_heartbeat_counter(__attribute__((unused)) value unit) {
  return caml_alloc_small(1, No_scan_tag);
}

CAMLprim value parallel_start_heartbeating(__attribute__((unused)) value interval_us) {
  return Val_unit;
}

#endif
