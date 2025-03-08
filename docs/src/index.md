```@setup inference_test
using UnrolledUtilities
```

#  UnrolledUtilities.jl

A toolkit for low-level optimization of Julia code in which iterator sizes are
known during compilation.

This package can be used with all *statically sized* iterators (`Tuple`s,
`NamedTuple`s, [`StaticArray`s](https://github.com/JuliaArrays/StaticArrays.jl),
etc.), including ones that are very long or ones that have elements of different
types, both of which are cases that Julia's standard library often handles
inefficiently. For example, the standard libary function `in` performs worse
than this package's `unrolled_in` for `Tuple`s with elements of different types:

```@repl inference_test
@allocated () in ((1, 2), (1, 2, 3))
@allocated unrolled_in((), ((1, 2), (1, 2, 3)))
```

The [loop unrolling](https://en.wikipedia.org/wiki/Loop_unrolling) automatically
performed by this package offers the following benefits for statically sized
iterators:
- better support for *static compilation*
  - compilation of [executables](https://github.com/tshort/StaticCompiler.jl)
  - compilation of [GPU kernels](https://github.com/JuliaGPU/CUDA.jl)
- better performance (usually)
  - reduced run times
  - reduced memory footprints while code is running
- better compilation efficiency (occasionally)
  - reduced compilation times
  - reduced memory footprints while code is compiling

To find out more about loop unrolling and when it is useful, see the
[Introduction](introduction.md).

## Package Features

This package exports a number of analogues to functions from `Base` and
`Base.Iterators`, each of which has been optimized for statically sized
iterators (in terms of both performance and compilation time):
- `unrolled_any(f, itr)`—similar to `any`
- `unrolled_all(f, itr)`—similar to `all`
- `unrolled_foreach(f, itrs...)`—similar to `foreach`
- `unrolled_map(f, itrs...)`—similar to `map`
- `unrolled_reduce(op, itr; [init])`—similar to `reduce`
- `unrolled_mapreduce(f, op, itrs...; [init])`—similar to `mapreduce`
- `unrolled_accumulate(op, itr; [init], [transform])`—similar to `accumulate`,
  but with a `transform` that can be applied to every value in the output
- `unrolled_push(itr, item)`—similar to `push!`, but non-mutating
- `unrolled_append(itr1, itr2)`—similar to `append!`, but non-mutating
- `unrolled_take(itr, ::Val{N})`—similar to `Iterators.take` (i.e., `itr[1:N]`),
  but with `N` wrapped in a `Val`
- `unrolled_drop(itr, ::Val{N})`—similar to `Iterators.drop` (i.e.,
  `itr[(N + 1):end]`), but with `N` wrapped in a `Val`
- `unrolled_in(item, itr)`—similar to `in`
- `unrolled_unique(itr)`—similar to `unique`
- `unrolled_filter(f, itr)`—similar to `filter`
- `unrolled_flatten(itr)`—similar to `Iterators.flatten`
- `unrolled_flatmap(f, itrs...)`—similar to `Iterators.flatmap`
- `unrolled_product(itrs...)`—similar to `Iterators.product`

In addition, this package exports two functions that do not have public
analogues in `Base` or `Base.Iterators`:
- `unrolled_applyat(f, n, itrs...)`—similar to `f(itrs[1][n], itrs[2][n], ...)`,
  but with a `Core.Const` index in every call to `getindex`
- `unrolled_split(f, itr)`—similar to `(filter(f, itr), filter(!f, itr))`, but
  without duplicate calls to `f`

These unrolled functions are compatible with the following types of iterators:
- statically sized iterators from `Base` (e.g., `Tuple` and `NamedTuple`)
- statically sized iterators from `StaticArrays` (e.g., `SVector` and `MVector`)
- lazy iterators from `Base` (e.g., the results of `enumerate`, `zip`,
  `Iterators.map`, and generator expressions) that are used as wrappers for
  statically sized iterators

They are also compatible with two new types of statically sized iterators
exported by this package:
- `StaticOneTo`—similar to `Base.OneTo`
- `StaticBitVector`—similar to `BitVector`

See the [User Guide](@ref "When to Use StaticOneTo and StaticBitVector") for
additional information about these new types of iterators.

See the [Developer Guide](@ref "How to Use the Interface") to learn how
user-defined iterator types can be made compatible with unrolled functions.
