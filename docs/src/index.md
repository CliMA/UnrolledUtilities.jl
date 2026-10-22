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

Functions from `Base` lose type stability on long heterogeneous `Tuple`s, and
`Tuple`s of more than 32 `Bool`s allocate when they are mapped; the unrolled
functions and a 300-bit [`StaticBitVector`](@ref) do neither:

```@repl quick_example
using UnrolledUtilities, Test
items = ntuple(n -> isodd(n) ? (n, n) : (n / 2,), 33);
Test.@inferred sum(sum, items);
Test.@inferred unrolled_sum(sum, items)
const bools = ntuple(isodd, Val(300));
const bits = StaticBitVector{300}(isodd);
map(!, bools) # hide
@allocated map(!, bools)
unrolled_map(!, bits) # hide
@allocated unrolled_map(!, bits)
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
