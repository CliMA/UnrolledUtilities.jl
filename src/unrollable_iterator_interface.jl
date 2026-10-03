"""
    generic_getindex(itr, n)

Return the `n`-th item of `itr`. Equivalent to `getindex(itr, n)`, with added
methods for the lazy iterators `Base.Generator`, `Iterators.Reverse`,
`Iterators.Enumerate`, and `Iterators.Zip`. Statically sized iterators that do
not support `getindex` can add methods to this function instead.

See also [`StaticSequence`](@ref), [`output_type_for_promotion`](@ref).
"""
Base.@propagate_inbounds generic_getindex(itr, n) = getindex(itr, n)
Base.@propagate_inbounds generic_getindex(itr::Base.Generator, n) =
    itr.f(generic_getindex(itr.iter, n))
Base.@propagate_inbounds generic_getindex(itr::Iterators.Reverse, n) =
    generic_getindex(itr.itr, length(itr.itr) - n + 1)
Base.@propagate_inbounds generic_getindex(itr::Iterators.Enumerate, n) =
    (n, generic_getindex(itr.itr, n))
Base.@propagate_inbounds generic_getindex(itr::Iterators.Zip, n) = unrolled_map(
    itr_k -> (Base.@_propagate_inbounds_meta; generic_getindex(itr_k, n)),
    itr.is,
)

@inline eltype_for_promotion(itr::Union{Tuple, NamedTuple}) =
    eltype(typeof(itr))
@inline eltype_for_promotion(itr::Base.Generator) =
    isempty(itr.iter) ? Union{} :
    Base.promote_op(itr.f, eltype_for_promotion(itr.iter))
@inline eltype_for_promotion(itr::Iterators.Reverse) =
    eltype_for_promotion(itr.itr)
@inline eltype_for_promotion(itr::Iterators.Enumerate) =
    Tuple{Int, eltype_for_promotion(itr.itr)}
@inline eltype_for_promotion(itr::Iterators.Zip) =
    Tuple{unrolled_map_into_tuple(eltype_for_promotion, itr.is)...}
@inline eltype_for_promotion(itr) =
    Base.promote_op(Base.Fix2(generic_getindex, 1), typeof(itr))

"""
    output_type_for_promotion(itr)

Return the type of container that unrolled functions construct for the input
`itr`: a `Type`, a [`ConditionalOutputType`](@ref) when the container type
depends on the item type, or [`NoOutputType()`](@ref) when `itr` has no
container type of its own. Defaults to `Tuple`, and lazy iterators from `Base`
forward to the iterators they wrap.

See also [`output_promote_rule`](@ref), [`constructor_from_tuple`](@ref).
"""
@inline output_type_for_promotion(_) = Tuple
@inline output_type_for_promotion(::NamedTuple{names}) where {names} =
    NamedTuple{names}
@inline output_type_for_promotion(itr::Base.Generator) =
    output_type_for_promotion(itr.iter)
@inline output_type_for_promotion(itr::Iterators.Reverse) =
    output_type_for_promotion(itr.itr)
@inline output_type_for_promotion(itr::Iterators.Enumerate) =
    output_type_for_promotion(itr.itr)
@inline output_type_for_promotion(itr::Iterators.Zip) =
    maybe_ambiguous_promoted_output_type(itr.is...)

"""
    AmbiguousOutputType

Abstract supertype for the results of [`output_type_for_promotion`](@ref) that
are not `Type`s: [`NoOutputType`](@ref) and [`ConditionalOutputType`](@ref).
"""
abstract type AmbiguousOutputType end

"""
    NoOutputType()

The [`AmbiguousOutputType`](@ref) of iterators with no container type of their
own, such as [`StaticOneTo`](@ref). It is promoted to any other output type,
and it becomes `Tuple` on its own.
"""
struct NoOutputType <: AmbiguousOutputType end

"""
    ConditionalOutputType(allowed_item_type, output_type, [fallback_type])

An [`AmbiguousOutputType`](@ref) that resolves to `output_type` when the item
type of the output is a subtype of `allowed_item_type`, and to `fallback_type`
(`Tuple` by default) otherwise. [`StaticBitVector`](@ref) uses it to fall back
to `Tuple` for items that are not `Bool`s.
"""
struct ConditionalOutputType{I, O, O′} <: AmbiguousOutputType end
@inline ConditionalOutputType(
    allowed_item_type::Type,
    output_type::Type,
    fallback_type::Type = Tuple,
) = ConditionalOutputType{allowed_item_type, output_type, fallback_type}()

"""
    output_promote_rule(output_type1, output_type2)

Return the output type for iterators whose [`output_type_for_promotion`](@ref)s
are `output_type1` and `output_type2`, or `Union{}` when the pair has no rule.
Only one direction needs a method; when both directions return `Union{}`, the
output type is `Tuple`.

By default, every type takes precedence over [`NoOutputType()`](@ref), and the
conditional part of a [`ConditionalOutputType`](@ref) is kept while its fallback
type is promoted.
"""
@inline output_promote_rule(_, _) = Union{}
@inline output_promote_rule(::Type{O}, ::Type{O}) where {O} = O
@inline output_promote_rule(::NoOutputType, output_type) = output_type

@inline output_promote_rule(
    ::ConditionalOutputType{I, O, O′},
    ::Type{O′′},
) where {I, O, O′, O′′} =
    ConditionalOutputType(I, O, output_promote_result(O′, O′′))
@inline output_promote_rule(
    ::Type{O′},
    ::ConditionalOutputType{I, O, O′′},
) where {I, O, O′, O′′} =
    ConditionalOutputType(I, O, output_promote_result(O′, O′′))
@inline output_promote_rule(
    ::ConditionalOutputType{I, O, O′},
    ::ConditionalOutputType{I, O, O′′},
) where {I, O, O′, O′′} =
    ConditionalOutputType(I, O, output_promote_result(O′, O′′))

@inline function output_promote_result(O1, O2)
    O12 = output_promote_rule(O1, O2)
    O21 = output_promote_rule(O2, O1)
    O12 == O21 == Union{} && return Tuple
    (O12 == O21 || O21 == Union{}) && return O12
    O12 == Union{} && return O21
    error("output_promote_rule yields inconsistent results for $O1 and $O2: \
           $O12 for $O1 followed by $O2, versus $O21 for $O2 followed by $O1")
end

@inline maybe_ambiguous_promoted_output_type() = Tuple
@inline maybe_ambiguous_promoted_output_type(itr) =
    output_type_for_promotion(itr)
@inline maybe_ambiguous_promoted_output_type(itr1, itr2) =
    output_promote_result(
        output_type_for_promotion(itr1),
        output_type_for_promotion(itr2),
    )
@inline maybe_ambiguous_promoted_output_type(itrs...) =
    fused_mapreduce(output_type_for_promotion, output_promote_result, itrs)

@inline _inferred_output_type(::Type{O}, _) where {O} = O
@inline _inferred_output_type(::NoOutputType, _) = Tuple
@inline _inferred_output_type(
    ::ConditionalOutputType{I, O, O′},
    itr,
) where {I, O, O′} = eltype_for_promotion(itr) <: I ? O : O′
@inline inferred_output_type(itr) =
    _inferred_output_type(output_type_for_promotion(itr), itr)

@inline _inferred_output_type(::Type{O}, _, _) where {O} = O
@inline _inferred_output_type(::NoOutputType, _, _) = Tuple
@inline _inferred_output_type(
    ::ConditionalOutputType{I, O, O′},
    itr,
    item,
) where {I, O, O′} =
    Union{eltype_for_promotion(itr), typeof(item)} <: I ? O : O′
@inline inferred_output_type(itr, item) =
    _inferred_output_type(output_type_for_promotion(itr), itr, item)

@inline union_types(::Type{T1}, ::Type{T2}) where {T1, T2} = Union{T1, T2}

@inline _promoted_output_type(::Type{O}, _) where {O} = O
@inline _promoted_output_type(::NoOutputType, _) = Tuple
@inline _promoted_output_type(
    ::ConditionalOutputType{I, O, O′},
    itrs,
) where {I, O, O′} =
    fused_mapreduce(eltype_for_promotion, union_types, itrs) <: I ? O : O′
@inline promoted_output_type(itrs...) =
    _promoted_output_type(maybe_ambiguous_promoted_output_type(itrs...), itrs)

@inline _unrolled_accumulate_output_type(
    ::Type{O},
    op::F,
    itr,
    init,
) where {O, F} = O
@inline _unrolled_accumulate_output_type(
    ::NoOutputType,
    op::F,
    itr,
    init,
) where {F} = Tuple
@inline function _unrolled_accumulate_output_type(
    ::ConditionalOutputType{I, O, O′},
    op::F,
    itr,
    init,
) where {I, O, O′, F}
    item_type = eltype_for_promotion(itr)
    acc_type =
        init isa NoInit ?
        Base.promote_op(reduction_first, typeof(op), item_type) : typeof(init)
    out_type =
        init isa NoInit && length(itr) <= 1 ? acc_type :
        Union{acc_type, Base.promote_op(op, acc_type, item_type)}
    return out_type <: I ? O : O′
end
@inline unrolled_accumulate_output_type(op::F, itr, init) where {F} =
    _unrolled_accumulate_output_type(
        output_type_for_promotion(itr),
        op,
        itr,
        init,
    )

"""
    constructor_from_tuple(output_type)

Return a function that constructs a container of type `output_type` from a
`Tuple` of items. Defaults to `identity`, which suits `Tuple`s and containers
that wrap them, such as `SVector`s. For `NamedTuple` types, the function
constructs a `NamedTuple` when the number of items matches the number of field
names, and it returns the `Tuple` otherwise.

Container types that cannot be constructed from a `Tuple` efficiently can add
methods to the `*_into` functions listed in the
[Developer Guide](@ref "How to Use the Interface") instead.

See also [`empty_output`](@ref).
"""
@inline constructor_from_tuple(::Type) = identity
@inline constructor_from_tuple(
    ::Type{NT},
) where {names, NT <: NamedTuple{names}} =
    items -> begin
        @inline
        length(items) == length(names) ? NT(items) : items
    end

"""
    empty_output(output_type)

Return an empty container of type `output_type`. Defaults to
`constructor_from_tuple(output_type)(())`, and returns `(;)` for `NamedTuple`
types.
"""
@inline empty_output(output_type) = constructor_from_tuple(output_type)(())
@inline empty_output(::Type{<:NamedTuple}) = (;)

@inline inferred_empty(itr) = empty_output(inferred_output_type(itr))

# This makes lazy iterators non-lazy, and it is a no-op for non-lazy iterators.
Base.@propagate_inbounds non_lazy_iterator(itr::Union{Tuple, NamedTuple}) = itr
Base.@propagate_inbounds non_lazy_iterator(itr) =
    unrolled_map_into(inferred_output_type(itr), identity, itr)
