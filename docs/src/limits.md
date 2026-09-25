```@meta
CurrentModule = UnrolledUtilities
```

```@setup limits
using UnrolledUtilities, Test
```

# Compilation Limits

Unrolled functions remove type instabilities only when the compiler can
determine the loop length and the type of every intermediate value. The limits
below are checked by `test/compilation_limits.jl`, which compiles every exported
function for the CPU and for an NVIDIA GPU.

## Loop lengths must be known during compilation

Every unrolled loop takes its length from the type of an iterator, from a `Val`,
or from a constant that the compiler propagates into the call. A length that
only exists at run time makes the result type `Any`:

```@repl limits
runtime_sum(n) = unrolled_sum(StaticOneTo(n));
constant_sum(::Val{n}) where {n} = unrolled_sum(StaticOneTo(n));
Base.return_types(runtime_sum, (Int,))
Base.return_types(constant_sum, (Val{9},))
```

Constants propagate only through functions that the compiler inlines. A literal
length passed through a function that is not inlined behaves like a run-time
length, so lengths that must reach an unrolled function are best passed as
`Val`s or as `StaticOneTo`s built from type parameters.

## Predicates with run-time values

[`unrolled_filter`](@ref), [`unrolled_split`](@ref), [`unrolled_unique`](@ref),
and [`unrolled_argfirst`](@ref) return containers or items whose types depend
on the values of a predicate. When the predicate constant-folds, as `x isa Int`
does, the result type is inferred. When it depends on run-time data, the result
type is a `Union` over the possible outcomes, and the compiler keeps the `Union`
only up to a small number of item types:

```@repl limits
num_filtered(itr) = length(unrolled_filter(_ -> rand(Bool), itr));
num_filtered((1, 2)) # hide
@allocated num_filtered((1, 2))
num_filtered((1, 2, 3)) # hide
@allocated num_filtered((1, 2, 3))
```

On GPUs, a `Union` of more than three item types cannot be compiled at all, and
the tests find lower limits for some inputs: `unrolled_filter` keeps a `Union`
for two run-time `Bool`s, and `unrolled_unique` for three. The remedy is a
predicate that depends only on types or on values known during compilation,
such as [`unrolled_in`](@ref) against a `Tuple` of `Symbol`s.

## Recursion limits

Functions that call themselves with different argument types trigger the
compiler's recursion limit, which widens types and introduces allocations. The
limit is removed from every function in this package, but it cannot be removed
from functions in `Base`, so recursive code that must stay type-stable has to
call unrolled functions at every level. The
[User Guide](@ref "Functions with recursion during compilation") shows an
example.

## Error paths in GPU kernels

An unrolled function that throws stays compilable on GPUs when the error
message is a constant, as in `error("invalid input")`. A message built at run
time, as in `error("invalid input $x")`, requires a string allocation, which
GPU kernels cannot perform. The same applies to callables passed to unrolled
functions.
