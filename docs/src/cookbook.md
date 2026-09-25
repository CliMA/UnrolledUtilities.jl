```@meta
CurrentModule = UnrolledUtilities
```

```@setup cookbook
using UnrolledUtilities, InteractiveUtils, Test
```

# Cookbook

Short recipes for the situations in which unrolled functions are most often
needed. Each recipe runs when the documentation is built, so the outputs shown
here come from the current version of the package.

## Transform the fields of a `NamedTuple`

[`unrolled_map`](@ref) keeps the field names of a `NamedTuple`, and reductions
run over its values:

```@repl cookbook
state = (; ρ = 1.2, u = 3.0, θ = 300.0);
unrolled_map(x -> 2x, state)
unrolled_sum(abs, state)
```

Functions that change the number of items, such as [`unrolled_filter`](@ref),
take the values of a `NamedTuple` as a `Tuple`:

```@repl cookbook
unrolled_filter(x -> x > 2, values(state))
```

## Compute a type parameter from a `Tuple`

The results of unrolled functions constant-fold when their inputs are known
during compilation, so they can be wrapped in `Val` and used as type parameters.
Here, the field names that start with a density are selected with
[`unrolled_filter`](@ref) and a predicate that constant-folds:

```@repl cookbook
density_names(::Val{names}) where {names} =
    Val(unrolled_filter(name -> unrolled_in(name, (:ρ, :ρe)), names));
Test.@inferred density_names(Val((:ρ, :u, :ρe, :θ)))
```

The predicate uses [`unrolled_in`](@ref), which compares with `===`. A predicate
whose value depends on run-time data does not constant-fold, and the result
length then cannot be inferred; see [Compilation Limits](limits.md).

## Index a heterogeneous `Tuple` with a run-time index

Indexing a `Tuple` with items of different types at a run-time index returns a
`Union` of their types, and most operations on the result then allocate.
[`unrolled_applyat`](@ref) instead applies the function in a separate branch
for each index, which is compiled to a `switch` instruction:

```@repl cookbook
items = (1, 2.0, 3.0f0, (4, 5));
count_bytes(itr, n) = unrolled_applyat(sizeof, n, itr);
count_bytes(items, 4) # hide
@allocated count_bytes(items, 4)
```

This is the pattern to use inside GPU kernels, where a `Union` return type
cannot be compiled.

## Read through a bounds-checked accessor without bounds checks

Every exported function propagates `@inbounds` from its call site to iterator
indexing and to callables marked with `Base.@propagate_inbounds`. To read from
an array inside such a function without bounds checks, mark the callable with
that annotation, which also works for anonymous functions:

```@repl cookbook
total(x) =
    @inbounds unrolled_sum(Base.@propagate_inbounds(i -> x[i]), StaticOneTo(8))
total_checked(x) = @inbounds unrolled_sum(i -> x[i], StaticOneTo(8))
has_bounds_checks(f) =
    occursin("throw_boundserror", sprint(code_llvm, f, Tuple{Vector{Float64}}))
total(collect(1.0:8.0))
has_bounds_checks(total)
has_bounds_checks(total_checked)
```

Without the annotation, the reads in `total_checked` keep their bounds checks
even though the call is marked `@inbounds`; see
[Constant Folding and Bounds Checks](@ref).

## Update a bitmask across loop iterations

A [`StaticBitVector`](@ref) stores up to 256 `Bool`s in 32 bytes, so it can be
modified in a loop without allocations, where a `Tuple` of more than 32 `Bool`s
would allocate:

```@repl cookbook
function mark_evens(n_items)
    mask = StaticBitVector{40}(false)
    for i in 1:n_items
        iseven(i) && (mask = Base.setindex(mask, true, i))
    end
    return mask
end
mask = mark_evens(40);
unrolled_count(mask), unrolled_findfirst(mask)
mark_evens(40) # hide
@allocated mark_evens(40)
```

`Base.setindex` takes a run-time index; [`unrolled_setindex`](@ref) takes a
compile-time index as a `Val`.
