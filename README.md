<h1 align="center">
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/src/assets/logo-dark.png">
  <source media="(prefers-color-scheme: light)" srcset="docs/src/assets/logo.png">
  <img alt="Shows the logo of UnrolledUtilities.jl" src="docs/src/assets/logo.png" width="480px">
</picture>
</h1>

# UnrolledUtilities.jl

Compile-time loop unrolling for statically sized iterators in Julia, built for type-stable, allocation-free execution on CPUs and GPUs.

`UnrolledUtilities.jl` provides unrolled analogues of functions from `Base` and `Base.Iterators` for statically sized iterators (`Tuple`, `NamedTuple`, `StaticArrays.SVector`, `StaticArrays.MVector`, [`StaticOneTo`](https://CliMA.github.io/UnrolledUtilities.jl/dev/api/#UnrolledUtilities.StaticOneTo), and [`StaticBitVector`](https://CliMA.github.io/UnrolledUtilities.jl/dev/api/#UnrolledUtilities.StaticBitVector)). It avoids Julia's 32-element tuple-splat limit (`MAX_TUPLE_SPLAT`) and recursion-limiting heuristics, enabling GPU kernel compilation (`GPUCompiler.jl` / `CUDA.jl`) and compile-time constant folding across heterogeneous and wide containers.

|||
|------------------:|:------------------------------------------------------------|
| **Documentation** | [![stable][docs-stable-img]][docs-stable-url] [![dev][docs-dev-img]][docs-dev-url] |
| **Version**       | [![version][version-img]][version-url]                      |
| **Docs Build**    | [![docs build][docs-bld-img]][docs-bld-url]                 |
| **License**       | [![license][license-img]][license-url]                      |
| **Tests**         | [![gha ci][gha-ci-img]][gha-ci-url]                         |
| **Code Coverage** | [![codecov][codecov-img]][codecov-url]                      |

[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-stable-url]: https://CliMA.github.io/UnrolledUtilities.jl/stable/

[docs-dev-img]: https://img.shields.io/badge/docs-dev-blue.svg
[docs-dev-url]: https://CliMA.github.io/UnrolledUtilities.jl/dev/

[version-img]: https://img.shields.io/github/v/tag/CliMA/UnrolledUtilities.jl?label=version
[version-url]: https://github.com/CliMA/UnrolledUtilities.jl/releases

[docs-bld-img]: https://github.com/CliMA/UnrolledUtilities.jl/actions/workflows/Documentation.yml/badge.svg?branch=main
[docs-bld-url]: https://github.com/CliMA/UnrolledUtilities.jl/actions/workflows/Documentation.yml?query=branch%3Amain

[license-img]: https://img.shields.io/badge/license-MIT-blue.svg
[license-url]: https://github.com/CliMA/UnrolledUtilities.jl/blob/main/LICENSE

[gha-ci-img]: https://github.com/CliMA/UnrolledUtilities.jl/actions/workflows/ci.yml/badge.svg?branch=main
[gha-ci-url]: https://github.com/CliMA/UnrolledUtilities.jl/actions/workflows/ci.yml?query=branch%3Amain

[codecov-img]: https://codecov.io/gh/CliMA/UnrolledUtilities.jl/branch/main/graph/badge.svg
[codecov-url]: https://codecov.io/gh/CliMA/UnrolledUtilities.jl

## Features

- **GPU-safe heterogeneous iteration**: Unrolls map, reduction, scan, search, and indexing operations without dynamic dispatch or heap allocation (`gpu_gc_pool_alloc`).
- **Scales beyond 32 elements**: Avoids Julia's 32-element tuple-splat limit and disables compiler recursion-widening heuristics across nested unrolled calls.
- **Container type preservation**: Preserves `Tuple`, `NamedTuple` (when field structure is preserved), `SVector`, `MVector`, and `StaticBitVector` (when output elements are `Bool`).
- **Guaranteed inlining**: Inlines every operation, so that results constant-fold whenever the compiler knows the inputs (as for `Val`s, or for `Tuple`s of `Symbol`s that are constant-propagated) and can be used as type parameters.
- **Compact static sequences**: Includes `StaticOneTo(N)` for zero-storage `1:N` ranges and `StaticBitVector{N, U}` for word-packed bitmasks, which can be updated in run-time loops without allocations up to 256 bits, and mapped, counted, and reduced word by word beyond that.

## Installation

Install `UnrolledUtilities.jl` from the Julia General registry:

```julia
using Pkg
Pkg.add("UnrolledUtilities")
```

## Quick Example

Functions from `Base` lose type stability on long heterogeneous `Tuple`s, and `Tuple`s of more than 32 `Bool`s allocate when they are mapped. The unrolled functions and a 300-bit `StaticBitVector` do neither:

```julia
using UnrolledUtilities, Test

# A Tuple of 33 Tuples with two different element types
items = ntuple(n -> isodd(n) ? (n, n) : (n / 2,), 33)
@test_throws ErrorException @inferred sum(sum, items) # Union{Float64, Int64}
@test (@inferred unrolled_sum(sum, items)) === 714.0

# 300 Bools as a Tuple and as a word-packed StaticBitVector
const bools = ntuple(isodd, Val(300))
const bits = StaticBitVector{300}(isodd)
@test (@allocated map(!, bools)) > 0
@test (@allocated unrolled_map(!, bits)) == 0
```

## Summary of Exported Operations

| Category | Exported Symbols | `Base` / `Iterators` Analogue & Description |
|:---|:---|:---|
| **Static Iterators** | `StaticSequence{N}`, `StaticOneTo(N)`, `StaticBitVector{N, U}` | Abstract static sequence, zero-storage `Base.OneTo(N)`, and word-packed static `BitVector`. |
| **Modification** | `unrolled_push`, `unrolled_append`, `unrolled_prepend` | Non-mutating analogues of `push!`, `append!`, and `prepend!`. |
| **Slicing & Indexing** | `unrolled_take`, `unrolled_drop`, `unrolled_setindex`, `unrolled_insert`, `unrolled_applyat` | Static-length `Iterators.take`, `Iterators.drop`, `Base.setindex`, non-mutating `insert!`, and switch-based `f(itr[n])`. |
| **Mapping & Traversal** | `unrolled_map`, `unrolled_foreach`, `unrolled_flatmap`, `unrolled_flatten` | Unrolled `map`, `foreach`, `Iterators.flatmap`, and `Iterators.flatten`. |
| **Filtering & Splitting** | `unrolled_filter`, `unrolled_split`, `unrolled_unique` | Unrolled `filter`, single-pass `(filter(f, itr), filter(!f, itr))`, and `unique`. |
| **Combinatorics** | `unrolled_product`, `unrolled_cycle`, `unrolled_partition` | Unrolled `Iterators.product`, `Iterators.cycle`, and `Iterators.partition`. |
| **Reductions & Scans** | `unrolled_reduce`, `unrolled_mapreduce`, `unrolled_accumulate`, `unrolled_sum`, `unrolled_prod`, `unrolled_cumsum`, `unrolled_cumprod`, `unrolled_count` | Unrolled `reduce` (`foldl`), `mapreduce`, `accumulate`, `sum`, `prod`, `cumsum`, `cumprod`, and `count`. |
| **Queries** | `unrolled_any`, `unrolled_all`, `unrolled_in`, `unrolled_allunique`, `unrolled_allequal` | Unrolled `any`, `all`, `in` (via `===`), `allunique`, and `allequal`. |
| **Search** | `unrolled_maximum`, `unrolled_minimum`, `unrolled_extrema`, `unrolled_findmax`, `unrolled_findmin`, `unrolled_argmax`, `unrolled_argmin`, `unrolled_findfirst`, `unrolled_findlast`, `unrolled_argfirst`, `unrolled_arglast` | Unrolled `maximum`, `minimum`, `extrema`, `findmax`, `findmin`, `argmax`, `argmin`, `findfirst`, `findlast`, and first/last matching element values. |

## Documentation

- [**Stable Documentation**](https://CliMA.github.io/UnrolledUtilities.jl/stable/): Documentation for the most recently tagged release.
- [**Dev Documentation**](https://CliMA.github.io/UnrolledUtilities.jl/dev/): Documentation for the latest `main` branch.
- [**Introduction**](https://CliMA.github.io/UnrolledUtilities.jl/dev/introduction/): Overview of loop unrolling and compiler trade-offs in Julia.
- [**User Guide**](https://CliMA.github.io/UnrolledUtilities.jl/dev/user_guide/): When to use unrolled functions, `StaticOneTo`, and `StaticBitVector`.
- [**Cookbook**](https://CliMA.github.io/UnrolledUtilities.jl/dev/cookbook/): Recipes for `NamedTuple` fields, type parameters, run-time indices, bounds checks, and bitmasks.
- [**Compilation Limits**](https://CliMA.github.io/UnrolledUtilities.jl/dev/limits/): What the compiler must know for unrolled code to stay type-stable.
- [**API Reference**](https://CliMA.github.io/UnrolledUtilities.jl/dev/api/): Every exported function and type, and the container type each function returns.
- [**Developer Guide**](https://CliMA.github.io/UnrolledUtilities.jl/dev/developer_guide/): Extending the unrollable iterator interface for custom container types.

## Integration with CliMA Models

`UnrolledUtilities.jl` is used across the [CliMA](https://clima.caltech.edu) ecosystem, including [ClimaCore.jl](https://github.com/CliMA/ClimaCore.jl) and [ClimaAtmos.jl](https://github.com/CliMA/ClimaAtmos.jl), for unrolling field-component tuples, stencil loops, and matrix-field operations in CPU and GPU kernels.

## Contributing

Contributions follow the shared conventions and engineering standards in [CliMA/DeveloperGuides](https://github.com/CliMA/DeveloperGuides). Run `Pkg.test()` locally before opening a pull request.
