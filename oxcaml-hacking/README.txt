Build:
dune build --release

Run:
PARALLEL_BENCH_DOMAINS=8 BENCHMARKS_RUNNER=TRUE BENCH_LIB=parallel_bench ./_build/default/bin/main.exe -run-without-cross-library-inlining -quota 1s
