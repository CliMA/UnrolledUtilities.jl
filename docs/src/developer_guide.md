```@meta
CurrentModule = UnrolledUtilities
```

## How to Unroll

There are two general ways to implement loop unrolling in Julia—recursively
splatting iterator contents and manually generating unrolled expressions. For
example, a recursively unrolled version of the `foreach` function is

```julia
unrolled_foreach(f, itr) = _unrolled_foreach(f, itr...)
_unrolled_foreach(f) = nothing
_unrolled_foreach(f, item, items...) = (f(item); _unrolled_foreach(f, items...))
```

In contrast, a generatively unrolled implementation of this function looks like

```julia
unrolled_foreach(f, itr) = _unrolled_foreach(Val(length(itr)), f, itr)
@generated _unrolled_foreach(::Val{N}, f, itr) where {N} =
    Expr(:block, (:(f(generic_getindex(itr, $n))) for n in 1:N)..., nothing)
```

To switch between recursive and generative unrolling, this package defines the
following function:

```@docs
rec_unroll
```

!!! tip "Tip"
    Recursive loop unrolling can be enabled by redefining this function:

    ```julia
    rec_unroll(itr) = true
    ```

The default choice of generative unrolling is motivated by the benchmarks for
[Generative vs. Recursive Unrolling](@ref).

## Interface API

The functions exported by this package can be used with any statically sized
iterators, as long as those iterators make appropriate use of the following
interface:

```@docs
generic_getindex
output_type_for_promotion
AmbiguousOutputType
NoOutputType
ConditionalOutputType
output_promote_rule
constructor_from_tuple
empty_output
```

## How to Use the Interface

To unroll over a statically sized iterator of some user-defined type `T`, follow
these steps:
- To enable recursive unrolling, add a method for `iterate(::T, [state])`
- To enable generative unrolling, add a method for `getindex(::T, n)` (or for
  `generic_getindex(::T, n)` if `getindex` should not be defined for iterators
  of type `T`)
- If every unrolled function that needs to construct an iterator when given an
  iterator of type `T` can return a `Tuple` instead, stop here
- Otherwise, to return a non-`Tuple` iterator whenever it is efficient to do so,
  follow these steps:
    - Add a method for `output_type_for_promotion(::T) = O`, where `O` can be
      `T`, a supertype of `T`, some other `Type`, or an `AmbiguousOutputType`
    - If an iterator whose output type is `O` can be used together with an
      iterator whose output type is `O′`, add a method for
      `output_promote_rule(O, O′)`
    - If `O` is a `NoOutputType`, stop here
    - Otherwise, to handle the unambiguous output type `U` that underlies `O`
      (where `U` is equivalent to `O` unless `O` is a `ConditionalOutputType`),
      follow these steps:
        - If an iterator of type `U` can be efficiently constructed from a
          `Tuple`, add a method for `constructor_from_tuple(U)`
        - Otherwise, for each of the following functions, add a method if it can
          be implemented to construct an iterator of type `U` without first
          storing the iterator's contents in a `Tuple`:
            - `empty_output(U)`
            - `unrolled_map_into(U, f, itr)`
            - `unrolled_accumulate_into(U, op, itr, init, transform)`
            - `unrolled_push_into(U, itr, item)`
            - `unrolled_append_into(U, itr1, itr2)`
            - `unrolled_take_into(U, itr, val_N)`
            - `unrolled_drop_into(U, itr, val_N)`

!!! note "Note"
    When a relevant method for the interface is not defined, unrolled functions
    will typically fall back to using `Tuple`s instead of other iterator types.
