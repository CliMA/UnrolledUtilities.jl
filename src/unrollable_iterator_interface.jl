"""
    generic_getindex(itr, n)

Identical to `getindex(itr, n)`, but with the added ability to handle lazy
iterator types defined in the standard library, such as `Base.Generator` and
`Iterators.Enumerate`.
"""
@inline generic_getindex(itr, n) = getindex(itr, n)
@inline generic_getindex(itr::Base.Generator, n) =
    itr.f(generic_getindex(itr.iter, n))
@inline generic_getindex(itr::Iterators.Reverse, n) =
    generic_getindex(itr.itr, length(itr.itr) - n + 1)
@inline generic_getindex(itr::Iterators.Enumerate, n) =
    (n, generic_getindex(itr.itr, n))
@inline generic_getindex(itr::Iterators.Zip, n) =
    unrolled_map(Base.Fix2(generic_getindex, n), itr.is)

@inline eltype_for_promotion(itr::Union{Tuple, NamedTuple}) =
    eltype(typeof(itr))
@inline eltype_for_promotion(itr::Base.Generator) =
    isempty(itr.iter) ? Union{} :
    Base.promote_op(itr.f, eltype_for_promotion(itr.iter))
@inline eltype_for_promotion(itr::Iterators.Reverse) =
    eltype_for_promotion(itr.itr)
@inline eltype_for_promotion(itr::Iterators.Enumerate) =
    Tuple{Int, eltype_for_promotion(itr.itr)}
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

@inline unambiguous_output_type(_, ::Type{O}) where {O} = O
@inline unambiguous_output_type(_, ::NoOutputType) = Tuple
@inline unambiguous_output_type(
    get_item_type,
    ::ConditionalOutputType{I, O, O′},
) where {I, O, O′} = get_item_type() <: I ? O : O′

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

@inline maybe_ambiguous_promoted_output_type(itrs...) =
    isempty(itrs) ? Tuple : # Generate a Tuple when given 0 inputs.
    unrolled_mapreduce(output_type_for_promotion, output_promote_result, itrs)

@inline inferred_output_type(itr) =
    unambiguous_output_type(output_type_for_promotion(itr)) do
        @inline
        eltype_for_promotion(itr)
    end

@inline inferred_output_type(itr, item) =
    unambiguous_output_type(output_type_for_promotion(itr)) do
        @inline
        Union{eltype_for_promotion(itr), typeof(item)}
    end

@inline promoted_output_type(itrs...) =
    unambiguous_output_type(maybe_ambiguous_promoted_output_type(itrs...)) do
        @inline
        unrolled_mapreduce(
            eltype_for_promotion,
            (T1, T2) -> Union{T1, T2},
            itrs,
        )
    end

@inline unrolled_map_output_type(f, itr) =
    inferred_output_type(Iterators.map(f, itr))

@inline unrolled_accumulate_output_type(op, itr, init) =
    unambiguous_output_type(output_type_for_promotion(itr)) do
        @inline
        item_type = eltype_for_promotion(itr)
        acc_type = init isa NoInit ? item_type : typeof(init)
        init isa NoInit && length(itr) <= 1 ? acc_type :
        Union{acc_type, Base.promote_op(op, acc_type, item_type)}
    end

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
@inline non_lazy_iterator(itr) =
    unrolled_append_into(inferred_output_type(itr), itr, inferred_empty(itr))
