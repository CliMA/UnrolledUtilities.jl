```@meta
CurrentModule = UnrolledUtilities
```

# API Reference

| `Base` or `Base.Iterators` | `UnrolledUtilities` |
|:---|:---|
| `push!`, `append!`, `prepend!` | [`unrolled_push`](@ref), [`unrolled_append`](@ref), [`unrolled_prepend`](@ref) |
| `Iterators.take`, `Iterators.drop`, `Base.setindex`, `insert!` | [`unrolled_take`](@ref), [`unrolled_drop`](@ref), [`unrolled_setindex`](@ref), [`unrolled_insert`](@ref) |
| `map`, `foreach` | [`unrolled_map`](@ref), [`unrolled_foreach`](@ref) |
| `any`, `all`, `in`, `count` | [`unrolled_any`](@ref), [`unrolled_all`](@ref), [`unrolled_in`](@ref), [`unrolled_count`](@ref) |
| `foldl`, `mapreduce`, `accumulate` | [`unrolled_reduce`](@ref), [`unrolled_mapreduce`](@ref), [`unrolled_accumulate`](@ref) |
| `sum`, `prod`, `cumsum`, `cumprod` | [`unrolled_sum`](@ref), [`unrolled_prod`](@ref), [`unrolled_cumsum`](@ref), [`unrolled_cumprod`](@ref) |
| `maximum`, `minimum`, `extrema` | [`unrolled_maximum`](@ref), [`unrolled_minimum`](@ref), [`unrolled_extrema`](@ref) |
| `findmax`, `findmin`, `argmax`, `argmin` | [`unrolled_findmax`](@ref), [`unrolled_findmin`](@ref), [`unrolled_argmax`](@ref), [`unrolled_argmin`](@ref) |
| `findfirst`, `findlast` | [`unrolled_findfirst`](@ref), [`unrolled_findlast`](@ref) |
| `unique`, `allunique`, `allequal`, `filter` | [`unrolled_unique`](@ref), [`unrolled_allunique`](@ref), [`unrolled_allequal`](@ref), [`unrolled_filter`](@ref) |
| `Iterators.flatten`, `Iterators.flatmap`, `Iterators.product`, `Iterators.cycle`, `Iterators.partition` | [`unrolled_flatten`](@ref), [`unrolled_flatmap`](@ref), [`unrolled_product`](@ref), [`unrolled_cycle`](@ref), [`unrolled_partition`](@ref) |
| none | [`unrolled_applyat`](@ref), [`unrolled_argfirst`](@ref), [`unrolled_arglast`](@ref), [`unrolled_split`](@ref) |

## Supported Iterators

The exported functions support the following statically sized iterators:
- `Tuple`s and `NamedTuple`s.
- `SVector`s and `MVector`s from `StaticArrays`.
- Lazy iterators from `Base` that wrap statically sized iterators:
  `Iterators.map` (`Base.Generator`), `Iterators.reverse`, `enumerate`, and
  `zip`.
- [`StaticOneTo`](@ref) and [`StaticBitVector`](@ref), which are defined in
  this package. The
  [User Guide](@ref "When to Use StaticOneTo and StaticBitVector") explains
  when to use them.

Other iterator types can be supported through the interface described in the
[Developer Guide](@ref "How to Use the Interface").

## Output Container Types

Functions that return containers infer the container type from their inputs:
- `Tuple`s and lazy iterators give `Tuple`s.
- `SVector`s and `MVector`s give `SVector`s and `MVector`s.
- `NamedTuple`s keep their field names in [`unrolled_map`](@ref),
  [`unrolled_setindex`](@ref), [`unrolled_accumulate`](@ref),
  [`unrolled_cumsum`](@ref), [`unrolled_cumprod`](@ref),
  [`unrolled_take`](@ref), [`unrolled_drop`](@ref), and
  [`unrolled_partition`](@ref). Functions that can change the number of items
  give `NamedTuple`s only when the output has one item per field name, and
  `Tuple`s otherwise.
- `StaticBitVector`s give `StaticBitVector`s when every item of the output is a
  `Bool`, and `Tuple`s otherwise.
- `StaticOneTo`s give `Tuple`s, except in `unrolled_take`.
- Inputs with different container types give `Tuple`s, except that a
  `StaticOneTo` adopts the container type of the other inputs, and
  `StaticBitVector`s with the same word type combine into a `StaticBitVector`.

## Constant Folding and Bounds Checks

All exported functions constant-fold at the value level for compile-time
constant inputs, so their results can be used as type parameters.

Functions that map, reduce, search, or filter iterators propagate `@inbounds`
from their call sites to iterator indexing and to callables that are marked
with `Base.@propagate_inbounds`. Functions that only rearrange items
(`unrolled_push`, `unrolled_append`, `unrolled_prepend`, `unrolled_take`,
`unrolled_drop`, `unrolled_setindex`, `unrolled_insert`, `unrolled_flatten`,
`unrolled_product`, `unrolled_cycle`, and `unrolled_partition`) do not.

## Static Iterator Types

```@docs
StaticSequence
StaticOneTo
StaticBitVector
```

## Container Modification and Slicing

```@docs
unrolled_push
unrolled_append
unrolled_prepend
unrolled_take
unrolled_drop
unrolled_setindex
unrolled_insert
```

## Mapping, Iteration, and Reductions

```@docs
unrolled_map
unrolled_foreach
unrolled_any
unrolled_all
unrolled_reduce
unrolled_mapreduce
unrolled_accumulate
unrolled_sum
unrolled_prod
unrolled_cumsum
unrolled_cumprod
unrolled_count
```

## Extrema, Searching, and Indexing

```@docs
unrolled_maximum
unrolled_minimum
unrolled_extrema
unrolled_findmax
unrolled_findmin
unrolled_argmax
unrolled_argmin
unrolled_findfirst
unrolled_findlast
unrolled_argfirst
unrolled_arglast
unrolled_applyat
```

## Membership, Uniqueness, and Filtering

```@docs
unrolled_in
unrolled_unique
unrolled_allunique
unrolled_allequal
unrolled_filter
unrolled_split
```

## Combinators and Partitioning

```@docs
unrolled_flatten
unrolled_flatmap
unrolled_product
unrolled_cycle
unrolled_partition
```
