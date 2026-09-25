"""
    generic_getindex(itr, n)

Identical to `getindex(itr, n)`, but with the added ability to handle lazy
iterator types defined in the standard library, such as `Base.Generator` and
`Iterators.Enumerate`.
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

The type of output that unrolled functions should try to generate for the input
iterator `itr`, or a `ConditionalOutputType` if the output type depends on the
type of items that need to be stored in it, or `NoOutputType()` if `itr` is a
lazy iterator without any associated output type. Defaults to `Tuple`.
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

The result of `output_type_for_promotion` for iterators that do not have
well-defined output types.
"""
abstract type AmbiguousOutputType end

"""
    NoOutputType()

The `AmbiguousOutputType` of lazy iterators.
"""
struct NoOutputType <: AmbiguousOutputType end

"""
    ConditionalOutputType(allowed_item_type, output_type, [fallback_type])

An `AmbiguousOutputType` that can have one of two possible values. If the
promoted item type of the output is a subtype of `allowed_item_type`, the output
will have the type `output_type`; otherwise, it will have the type
`fallback_type`, which is set to `Tuple` by default.
"""
struct ConditionalOutputType{I, O, O′} <: AmbiguousOutputType end
@inline ConditionalOutputType(
    allowed_item_type::Type,
    output_type::Type,
    fallback_type::Type = Tuple,
) = ConditionalOutputType{allowed_item_type, output_type, fallback_type}()

"""
    output_promote_rule(output_type1, output_type2)

The type of output that should be generated when two iterators do not have the
same `output_type_for_promotion`, or `Union{}` if no custom promotion rule is
defined for this direction. Only one method of `output_promote_rule` needs to be
defined for any pair of output types; if both directions return `Union{}`,
`output_promote_result` falls back to `Tuple`.

By default, all types take precedence over `NoOutputType()`, and the conditional
part of any `ConditionalOutputType` takes precedence over an unconditional type
(so that only the `fallback_type` of any conditional type gets promoted).
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
    acc_type = init isa NoInit ? item_type : typeof(init)
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

A function that can be used to efficiently construct an output of type
`output_type` from a `Tuple`, or `identity` if such an output should not be
constructed from a `Tuple`. Defaults to `identity`, which also handles the case
where `output_type` is already `Tuple`. The `output_type` here is guaranteed to
be a `Type`, rather than a `ConditionalOutputType` or `NoOutputType`.

Many statically sized iterators (e.g., `SVector`s) are essentially wrappers for
`Tuple`s, and their constructors for `Tuple`s can be reduced to no-ops.
`NamedTuple` types construct a `NamedTuple` when the tuple length matches the
number of field names and fall back to `Tuple` otherwise.
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

An empty output of type `output_type`. Defaults to applying the
`constructor_from_tuple` for the given type to an empty `Tuple`.
"""
@inline empty_output(output_type) = constructor_from_tuple(output_type)(())
@inline empty_output(::Type{<:NamedTuple}) = (;)

@inline inferred_empty(itr) = empty_output(inferred_output_type(itr))

# This makes lazy iterators non-lazy, and it is a no-op for non-lazy iterators.
Base.@propagate_inbounds non_lazy_iterator(itr::Union{Tuple, NamedTuple}) = itr
Base.@propagate_inbounds non_lazy_iterator(itr) =
    unrolled_map_into(inferred_output_type(itr), identity, itr)
