"""
    StaticBitVector{N, [U]}(f)
    StaticBitVector{N, [U]}([bit])

A statically sized analogue of `BitVector` with `Unsigned` chunks of type `U`,
which can be constructed using either a function `f(n)` or a constant `bit`. By
default, `U` is set to `UInt8` and `bit` is set to `false`.

This iterator can only store `Bool`s, so its `output_type_for_promotion` is a
`ConditionalOutputType`. Efficient implementations are provided for all unrolled
functions; when all output items are `Bool`s, the output is a `StaticBitVector`,
and otherwise it falls back to `Tuple`.
"""
struct StaticBitVector{N, U <: Unsigned, I <: NTuple{<:Any, U}} <:
       StaticSequence{N}
    ints::I
end
@inline StaticBitVector{N, U}(ints::Tuple) where {N, U} =
    StaticBitVector{N, U, typeof(ints)}(ints)
@inline StaticBitVector{N}(args...) where {N} =
    StaticBitVector{N, UInt8}(args...)

@inline function StaticBitVector{N, U}(bit::Bool = false) where {N, U}
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = ntuple(Returns(bit ? ~zero(U) : zero(U)), Val(n_ints))
    return StaticBitVector{N, U}(ints)
end

@inline function StaticBitVector{N, U}(f) where {N, U}
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = ntuple(Val(n_ints)) do int_index
        @inline
        first_index = n_bits_per_int * (int_index - 1) + 1
        unrolled_reduce(StaticOneTo(n_bits_per_int), zero(U)) do int, bit_index
            @inline
            bit_offset = bit_index - 1
            index = first_index + bit_offset
            index <= N ? (int | U(f(index)::Bool) << bit_offset) : int
        end
    end
    return StaticBitVector{N, U}(ints)
end

@inline function int_index_and_bit_offset(::Type{U}, n) where {U}
    int_offset, bit_offset = divrem(n - 1, 8 * sizeof(U))
    return (int_offset + 1, bit_offset)
end

@inline function generic_getindex(
    itr::StaticBitVector{<:Any, U},
    n::Integer,
) where {U}
    int_index, bit_offset = int_index_and_bit_offset(U, n)
    int = itr.ints[int_index]
    return Bool(int >> bit_offset & one(int))
end

@inline function Base.setindex(
    itr::StaticBitVector{N, U},
    bit::Bool,
    n::Integer,
) where {N, U}
    int_index, bit_offset = int_index_and_bit_offset(U, n)
    int = itr.ints[int_index]
    new_int = int & ~(one(U) << bit_offset) | U(bit) << bit_offset
    ints = ntuple(
        k -> ifelse(k == int_index, new_int, itr.ints[k]),
        Val(length(itr.ints)),
    )
    return StaticBitVector{N, U}(ints)
end

@inline unrolled_setindex_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    bit::Bool,
    ::Val{N},
) where {N, U} =
    N < 1 || N > length(itr) ? Base.throw_boundserror(itr, N) :
    Base.setindex(itr, bit, N)

@inline eltype_for_promotion(::StaticBitVector) = Bool

@inline output_type_for_promotion(::StaticBitVector{<:Any, U}) where {U} =
    ConditionalOutputType(Bool, StaticBitVector{<:Any, U})

@inline constructor_from_tuple(::Type{StaticBitVector{<:Any, U}}) where {U} =
    items ->
        StaticBitVector{length(items), U}(Base.Fix1(generic_getindex, items))

@inline empty_output(::Type{StaticBitVector{<:Any, U}}) where {U} =
    StaticBitVector{0, U}()

@inline unrolled_map_into(::Type{StaticBitVector{<:Any, U}}, f, itr) where {U} =
    StaticBitVector{length(itr), U}(
        Base.Fix1(generic_getindex, Iterators.map(f, itr)),
    )

@inline function unrolled_push_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    bit::Bool,
) where {U}
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(length(itr), n_bits_per_int)
    bit_offset = length(itr) % n_bits_per_int
    ints = if bit_offset == 0
        unrolled_push(itr.ints, U(bit))
    else
        last_int = itr.ints[n_ints]
        new_last_int =
            last_int & ~(one(U) << bit_offset) | U(bit) << bit_offset
        unrolled_push(unrolled_take(itr.ints, Val(n_ints - 1)), new_last_int)
    end
    return StaticBitVector{length(itr) + 1, U}(ints)
end

@inline function unrolled_append_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr1::StaticBitVector{<:Any, U},
    itr2::StaticBitVector{<:Any, U},
) where {U}
    n_bits_per_int = 8 * sizeof(U)
    n_ints1 = cld(length(itr1), n_bits_per_int)
    bit_offset = length(itr1) % n_bits_per_int
    ints = if bit_offset == 0 || length(itr2) == 0
        unrolled_append(itr1.ints, itr2.ints)
    else
        mid_int1 = itr1.ints[n_ints1]
        mid_int2 = itr2.ints[1]
        mid_int =
            mid_int1 & ~(~zero(U) << bit_offset) | mid_int2 << bit_offset
        final_ints =
            length(itr2) + bit_offset <= n_bits_per_int ? () :
            unrolled_drop(itr2, Val(n_bits_per_int - bit_offset)).ints
        unrolled_append(
            unrolled_push(unrolled_take(itr1.ints, Val(n_ints1 - 1)), mid_int),
            final_ints,
        )
    end
    return StaticBitVector{length(itr1) + length(itr2), U}(ints)
end

@inline function unrolled_take_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    ::Val{N},
) where {N, U}
    (N < 0 || N > length(itr)) && Base.throw_boundserror(itr, N)
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = unrolled_take(itr.ints, Val(n_ints))
    return StaticBitVector{N, U}(ints)
end

@inline function unrolled_drop_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    ::Val{N},
) where {N, U}
    (N < 0 || N > length(itr)) && Base.throw_boundserror(itr, N)
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(length(itr) - N, n_bits_per_int)
    n_dropped_ints = fld(N, n_bits_per_int)
    bit_offset = N - n_bits_per_int * n_dropped_ints
    ints_without_offset = unrolled_drop(itr.ints, Val(n_dropped_ints))
    ints = if bit_offset == 0 || length(itr) <= N
        unrolled_take(ints_without_offset, Val(n_ints))
    else
        ntuple(Val(n_ints)) do k
            @inline
            cur_int = ints_without_offset[k]
            k == length(ints_without_offset) ? cur_int >> bit_offset :
            cur_int >> bit_offset |
            ints_without_offset[k + 1] << (n_bits_per_int - bit_offset)
        end
    end
    return StaticBitVector{length(itr) - N, U}(ints)
end

@inline unrolled_insert_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    bit::Bool,
    ::Val{N},
) where {N, U} =
    N < 1 || N > length(itr) + 1 ? Base.throw_boundserror(itr, N) :
    unrolled_append(
        unrolled_push(unrolled_take(itr, Val(N - 1)), bit),
        unrolled_drop(itr, Val(N - 1)),
    )

@inline function unrolled_accumulate_into(
    ::Type{StaticBitVector{<:Any, U}},
    op,
    itr,
    init,
) where {U}
    N = length(itr)
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = unrolled_accumulate(
        StaticOneTo(n_ints),
        (nothing, init),
    ) do (_, init_value_for_new_int), int_index
        @inline
        first_index = n_bits_per_int * (int_index - 1) + 1
        unrolled_reduce(
            StaticOneTo(n_bits_per_int),
            (zero(U), init_value_for_new_int),
        ) do (int, prev_value), bit_index
            @inline
            bit_offset = bit_index - 1
            index = first_index + bit_offset
            if index <= N
                item = generic_getindex(itr, index)
                new_value =
                    index == 1 && prev_value isa NoInit ? item :
                    op(prev_value, item)
                (int | U(new_value::Bool) << bit_offset, new_value)
            else
                (int, prev_value)
            end
        end
    end
    return StaticBitVector{N, U}(unrolled_map(first, ints))
end

@inline function _masked_ints(itr::StaticBitVector{N, U}) where {N, U}
    N == 0 && return ()
    rem_bits = N % (8 * sizeof(U))
    rem_bits == 0 && return itr.ints
    mask = (one(U) << rem_bits) - one(U)
    return ntuple(
        k -> ifelse(k == length(itr.ints), itr.ints[end] & mask, itr.ints[k]),
        Val(length(itr.ints)),
    )
end

# `~` also flips the unused bits of the last word, so they are cleared to keep
# the result identical to a vector constructed from the negated bits.
@inline function unrolled_map(
    ::typeof(!),
    itr::StaticBitVector{N, U},
) where {N, U}
    negated_itr = StaticBitVector{N, U}(unrolled_map(~, itr.ints))
    return StaticBitVector{N, U}(_masked_ints(negated_itr))
end

@inline unrolled_any(::typeof(identity), itr::StaticBitVector) =
    unrolled_any(!iszero, _masked_ints(itr))
@inline unrolled_any(::typeof(!), itr::StaticBitVector) =
    unrolled_any(identity, unrolled_map(!, itr))

@inline unrolled_all(::typeof(identity), itr::StaticBitVector) =
    !unrolled_any(!, itr)
@inline unrolled_all(::typeof(!), itr::StaticBitVector) =
    !unrolled_any(identity, itr)

@inline unrolled_count(::typeof(identity), itr::StaticBitVector) =
    Int(unrolled_sum(count_ones, _masked_ints(itr)))
@inline unrolled_count(::typeof(!), itr::StaticBitVector) =
    Int(unrolled_count(identity, unrolled_map(!, itr)))

@inline word_level_reduction(::typeof(&)) = unrolled_all
@inline word_level_reduction(::typeof(|)) = unrolled_any

@inline unrolled_reduce(
    op::Union{typeof(&), typeof(|)},
    itr::StaticBitVector,
    init,
) = unrolled_mapreduce(identity, op, itr; init)
@inline function unrolled_mapreduce(
    f::Union{typeof(identity), typeof(!)},
    op::Union{typeof(&), typeof(|)},
    itr::StaticBitVector;
    init = NoInit(),
)
    isempty(itr) && return empty_reduction_value(init)
    result = word_level_reduction(op)(f, itr)
    return init isa NoInit ? result : op(init, result)
end
