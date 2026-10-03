# UnrolledUtilities.jl

Compile-time loop unrolling for statically sized iterators, for type-stable and
allocation-free code on CPUs and GPUs.

```@docs
UnrolledUtilities
```

## Installation

```julia
using Pkg
Pkg.add("UnrolledUtilities")
```

## Quick Example

A heterogeneous `Tuple` can be reduced and indexed without allocations, and a
300-bit [`StaticBitVector`](@ref) can be counted without allocations:

```@repl quick_example
using UnrolledUtilities
const items = (1.5f0, 2, Val(3), (4.0f0, 5.0f0));
to_float(x::Number) = Float32(x);
to_float(::Val{N}) where {N} = Float32(N);
to_float(t::Tuple) = unrolled_sum(Float32, t);
unrolled_mapreduce(to_float, +, items)
apply_at_n(itr, n) = unrolled_applyat(to_float, n, itr);
apply_at_n(items, 4)
@allocated apply_at_n(items, 4)
const bits = StaticBitVector{300}(isodd);
unrolled_count(bits)
@allocated unrolled_count(bits)
```

## Contents

- [Introduction](introduction.md): what loop unrolling does, and what it
  costs.
- [User Guide](user_guide.md): when unrolled functions and the static iterator
  types outperform their counterparts in `Base`.
- [Cookbook](cookbook.md): short recipes for `NamedTuple` fields, type
  parameters, run-time indices, bounds checks, and bitmasks.
- [Compilation Limits](limits.md): what the compiler must know for unrolled
  code to stay type-stable, and how to give it that information.
- [API Reference](api.md): every exported function and type, and the container
  type that each function returns.
- [Developer Guide](developer_guide.md): how to make a user-defined iterator
  type work with the unrolled functions.
- [Comparison Tables](comparison_tables.md): measured run times, compilation
  times, and allocations of unrolled functions against their counterparts in
  `Base`.

## Contributing

Issues and pull requests are welcome on
[GitHub](https://github.com/CliMA/UnrolledUtilities.jl). Development follows the
[CliMA developer guides](https://github.com/CliMA/DeveloperGuides). `Pkg.test()`
runs the test suite in a few minutes; the benchmarks behind the comparison
tables run when `UNROLLED_UTILITIES_BENCHMARK=true` is set.
