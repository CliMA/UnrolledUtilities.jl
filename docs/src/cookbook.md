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
over its values work on `Tuple(nt)`:

```@repl cookbook
state = (; ρ = 1.2, u = 3.0, θ = 300.0);
unrolled_map(x -> 2x, state)
unrolled_sum(abs, Tuple(state))
```

To keep a subset of the fields, pass their names as a `Val`, so that the
selection is known during compilation and the result type is inferred:

```@repl cookbook
select(nt, ::Val{names}) where {names} =
    NamedTuple{names}(unrolled_map(name -> getproperty(nt, name), names));
Test.@inferred select(state, Val((:ρ, :θ)))
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

Functions that map, reduce, search, or filter propagate `@inbounds` from their
call site to iterator indexing and to callables marked with
`Base.@propagate_inbounds`. To read from an array inside such a function
without bounds checks, wrap the read in a callable struct with that annotation:

```@repl cookbook
struct Column{A}
    data::A
end
Base.@propagate_inbounds value(c::Column, i) = c.data[i]
struct ColumnReader{C}
    column::C
end
Base.@propagate_inbounds (r::ColumnReader)(i) = value(r.column, i)
total(c) = @inbounds unrolled_sum(ColumnReader(c), StaticOneTo(8));
total_checked(c) = unrolled_sum(ColumnReader(c), StaticOneTo(8));
column = Column(collect(1.0:8.0));
total(column)
occursin("throw_boundserror", sprint(code_llvm, total, Tuple{typeof(column)}))
occursin("throw_boundserror", sprint(code_llvm, total_checked, Tuple{typeof(column)}))
```

A closure such as `i -> value(c, i)` keeps its bounds checks, because closures
cannot carry the `Base.@propagate_inbounds` annotation. The functions that only
rearrange items, such as [`unrolled_take`](@ref), do not propagate `@inbounds`;
see [Constant Folding and Bounds Checks](@ref).

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
