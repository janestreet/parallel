  $ ulimit -c 0

  $ function test_on_panic () {
  >   { 
  >     ${TEST_DIR}/$1
  >   } 2>&1 | sed -E 's/.*(Aborted \s+\(core dumped\)).*/\1/g'
  > 
  >   echo ${PIPESTATUS[0]}
  > } 

  $ test_on_panic parallel_async_on_panic.exe 
  Fatal error: Panicked in on_panic, terminating program.
  Aborted                 (core dumped)
  134

  $ test_on_panic parallel_monitor_on_panic.exe
  Fatal error: Panicked in on_panic, terminating program.
  Aborted                 (core dumped)
  134
