UnrolledUtilities.jl Release Notes
==================================

main
----

v0.1.12
-------
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_insert` on `StaticBitVector`s, which inserted bits one position too late, and `unrolled_drop`, which left stale words that corrupted later `unrolled_push` and `unrolled_append` calls.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Changed `StaticBitVector` outputs to fall back to `Tuple`s when non-`Bool` items are added. `unrolled_push` truncated them to `Bool`s (for example, `unrolled_push(bv, 42)` stored `false`), and `unrolled_append` and `unrolled_setindex` threw errors.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_take(StaticOneTo(N), Val(M))` with `M > N`, which returned a `StaticOneTo{M}` instead of throwing a `BoundsError` as for `Tuple`s.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `NamedTuple` inputs, which threw errors in most functions that return containers. `unrolled_take`, `unrolled_drop`, `unrolled_setindex`, `unrolled_map`, and `unrolled_partition` preserve field names, and other functions return `Tuple`s when the field names cannot be preserved. Mixed `NamedTuple`, `Tuple`, and `SVector` inputs return `Tuple`s.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_count` on single-item inputs, which returned a `Bool` instead of an `Int`.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_sum` and `unrolled_prod`, which ignored `init` for non-empty inputs.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_findmax`, `unrolled_findmin`, `unrolled_argmax`, and `unrolled_argmin` to match `Base` for `NaN`, `missing`, and signed zeros.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Fixed `unrolled_unique` and `unrolled_allunique` to call their callable once per item.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Removed the compiler's recursion limits from all methods of the package and its StaticArrays extension, including closures, constructors, and keyword-argument methods.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Reduced compilation time by about 1.6x on wide heterogeneous tuples.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Added word-level `StaticBitVector` implementations of `unrolled_map(!, bv)`, `unrolled_any`, `unrolled_all`, `unrolled_count`, and `&`/`|` reductions, which compile 7x–35x faster and run up to 3x faster in GPU kernels. `StaticBitVector`s longer than 32 words run without allocations on CPU and GPU.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Made 31 exported functions propagate `@inbounds` from the call site to iterator indexing.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Added a `StaticBitVector` constructor from a `Tuple` of `Bool`s, and `StaticBitVector`s now print as that constructor call.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Changed the error for reducing an empty iterator without `init` to an `ArgumentError` that applies to every reduction function.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Added docstrings for all exported functions and types, an API reference, a cookbook, and a page on compilation limits to the documentation, and expanded the README.
- [#37](https://github.com/CliMA/UnrolledUtilities.jl/pull/37) Added CUDA.jl 6 support to the test suite and made its benchmarks opt-in via `UNROLLED_UTILITIES_BENCHMARK=true`.
