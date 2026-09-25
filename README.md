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
- **Value-level constant folding**: Constant-folds operations on compile-time constants (such as `Tuple`s of `Symbol`s or `Val`s) so results can be used directly as type parameters.
- **Compact static sequences**: Includes `StaticOneTo(N)` for zero-storage `1:N` ranges and `StaticBitVector{N, U}` for word-packed bitmasks that compile on GPUs past 256 bits without heap allocation.

## Installation

Install `UnrolledUtilities.jl` from the Julia General registry:

```julia
using Pkg
Pkg.add("UnrolledUtilities")
```

## Quick Example

Standard `Base` reductions and indexing over heterogeneous tuples or tuples with more than 32 elements can fail type inference, allocate on the heap, and fail to compile inside GPU kernels. `UnrolledUtilities.jl` resolves each element statically at compile time:

```julia
using UnrolledUtilities, Test

# Heterogeneous tuple mixing Float32, Int, and a compile-time Val
const items = (1.5f0, 2, Val(3), (4.0f0, 5.0f0))
to_float(x::Number) = Float32(x)
to_float(::Val{N}) where {N} = Float32(N)
to_float(t::Tuple) = unrolled_sum(Float32, t)

# Type-stable, zero-allocation mapreduce over heterogeneous elements
total = @inferred unrolled_mapreduce(to_float, +, items)
@test total === 15.5f0

# Runtime indexing into a heterogeneous tuple via a compiled switch table
apply_at_n(itr, n) = unrolled_applyat(to_float, n, itr)
@test (@inferred apply_at_n(items, 4)) === 9.0f0
@test (@allocated apply_at_n(items, 4)) == 0

# Word-packed static bitmask (>32 UInt8 words) with zero allocations
const bits = StaticBitVector{300}(isodd)
@test unrolled_count(bits) === 150
@test (@allocated unrolled_count(bits)) == 0
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
| **Extrema & Queries** | `unrolled_any`, `unrolled_all`, `unrolled_in`, `unrolled_allunique`, `unrolled_allequal`, `unrolled_maximum`, `unrolled_minimum`, `unrolled_extrema` | Unrolled `any`, `all`, `in` (via `===`), `allunique`, `allequal`, `maximum`, `minimum`, and `extrema`. |
| **Search & Argextrema** | `unrolled_findmax`, `unrolled_findmin`, `unrolled_argmax`, `unrolled_argmin`, `unrolled_findfirst`, `unrolled_findlast`, `unrolled_argfirst`, `unrolled_arglast` | Unrolled `findmax`, `findmin`, `argmax`, `argmin`, `findfirst`, `findlast`, and first/last matching element values. |

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
