"""
    StaticBitVector{N, U, I} <: StaticSequence{N}
    StaticBitVector{N, [U]}(bits::NTuple{N, Bool})
    StaticBitVector{N, [U]}(ints::Tuple)
    StaticBitVector{N, [U]}(f)
    StaticBitVector{N, [U]}([bit])

Analogue of `BitVector` with `N` `Bool`s packed into words of the `Unsigned`
type `U`, which defaults to `UInt8`. The bits are set from a `Tuple` of `Bool`s
`bits`, from a `Tuple` of `cld(N, 8 * sizeof(U))` words `ints`, from `f(n)` for
`n` in `1:N`, or to the constant `bit`, which defaults to `false`.

Unrolled functions have word-level implementations for `StaticBitVector`s. They
return a `StaticBitVector` when every item of the result is a `Bool`, and a
`Tuple` otherwise.

# Fields
- `ints::I`: `Tuple` of the `cld(N, 8 * sizeof(U))` words of type `U`.

# Examples
```julia
bv = StaticBitVector{3}(isodd) # StaticBitVector{3, UInt8}((true, false, true))
unrolled_count(bv)             # 2
unrolled_push(bv, true) # StaticBitVector{4, UInt8}((true, false, true, true))
unrolled_push(bv, 42)   # (true, false, true, 42)
```

See also [`StaticSequence`](@ref), [`StaticOneTo`](@ref).
"""
struct StaticBitVector{N, U <: Unsigned, I <: NTuple{<:Any, U}} <:
       StaticSequence{N}
    ints::I
end
# The word-level methods read words without bounds checks, so the number of
# words is checked here. The unused bits of the last word are cleared, so that
# vectors with the same bits are identical (===).
@inline function StaticBitVector{N, U}(ints::Tuple) where {N, U}
    length(ints) == cld(N, 8 * sizeof(U)) || throw(
        ArgumentError("the number of words must be cld(N, 8 * sizeof(U))"),
    )
    masked_ints = _masked_ints(Val(N), U, ints)
    return StaticBitVector{N, U, typeof(masked_ints)}(masked_ints)
end
@inline StaticBitVector{N}(args...) where {N} =
    StaticBitVector{N, UInt8}(args...)
# An empty Tuple is also an empty NTuple{N, Bool}, so the constructor from a
# callable builds the result with the inner constructor to avoid dispatching
# back to this method.
Base.@propagate_inbounds StaticBitVector{N, U}(
    bits::NTuple{N, Bool},
) where {N, U} = StaticBitVector{N, U}(
    n -> (Base.@_propagate_inbounds_meta; getindex(bits, n)),
)

# The printed form is the constructor call from a Tuple of Bools, which shows
# every bit and evaluates back to the same vector.
Base.show(io::IO, itr::StaticBitVector{N, U}) where {N, U} =
    print(io, "StaticBitVector{", N, ", ", U, "}(", Tuple(itr), ")")

@inline function _masked_ints(::Val{N}, ::Type{U}, ints::Tuple) where {N, U}
    N == 0 && return ()
    rem_bits = N % (8 * sizeof(U))
    rem_bits == 0 && return ints
    mask = (one(U) << rem_bits) - one(U)
    return ntuple(
        k -> ifelse(
            k == length(ints),
            @inbounds(ints[end]) & mask,
            @inbounds(ints[k]),
        ),
        Val(length(ints)),
    )
end
@inline _masked_ints(itr::StaticBitVector{N, U}) where {N, U} =
    _masked_ints(Val(N), U, itr.ints)

@inline function StaticBitVector{N, U}(bit::Bool = false) where {N, U}
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = _masked_ints(
        Val(N),
        U,
        ntuple(Returns(bit ? ~zero(U) : zero(U)), Val(n_ints)),
    )
    return StaticBitVector{N, U, typeof(ints)}(ints)
end

Base.@propagate_inbounds function StaticBitVector{N, U}(f::F) where {N, U, F}
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = unrolled_map_into_tuple(StaticOneTo(n_ints)) do int_index
        Base.@_propagate_inbounds_meta
        first_index = n_bits_per_int * (int_index - 1) + 1
        unrolled_reduce(StaticOneTo(n_bits_per_int), zero(U)) do int, bit_index
            Base.@_propagate_inbounds_meta
            bit_offset = bit_index - 1
            index = first_index + bit_offset
            index <= N ? (int | U(f(index)::Bool) << bit_offset) : int
        end
    end
    return StaticBitVector{N, U, typeof(ints)}(ints)
end

@inline function int_index_and_bit_offset(::Type{U}, n) where {U}
    int_offset, bit_offset = divrem(n - 1, 8 * sizeof(U))
    return (int_offset + 1, bit_offset)
end

Base.@propagate_inbounds function generic_getindex(
    itr::StaticBitVector{<:Any, U},
    n::Integer,
) where {U}
    int_index, bit_offset = int_index_and_bit_offset(U, n)
    int = itr.ints[int_index]
    return Bool(int >> bit_offset & one(int))
end

Base.@propagate_inbounds function Base.setindex(
    itr::StaticBitVector{N, U},
    bit::Bool,
    n::Integer,
) where {N, U}
    int_index, bit_offset = int_index_and_bit_offset(U, n)
    int = itr.ints[int_index]
    new_int = int & ~(one(U) << bit_offset) | U(bit) << bit_offset
    ints = ntuple(
        k -> ifelse(k == int_index, new_int, @inbounds(itr.ints[k])),
        Val(length(itr.ints)),
    )
    return StaticBitVector{N, U, typeof(ints)}(ints)
end

@inline unrolled_setindex_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    bit::Bool,
    ::Val{N},
) where {N, U} =
    N < 1 || N > length(itr) ? Base.throw_boundserror(itr, N) :
    @inbounds(Base.setindex(itr, bit, N))

@inline eltype_for_promotion(::StaticBitVector) = Bool

@inline output_type_for_promotion(::StaticBitVector{<:Any, U}) where {U} =
    ConditionalOutputType(Bool, StaticBitVector{<:Any, U})

@inline constructor_from_tuple(::Type{StaticBitVector{<:Any, U}}) where {U} =
    items -> (
        Base.@_propagate_inbounds_meta;
        StaticBitVector{length(items), U}(
            n -> (Base.@_propagate_inbounds_meta; generic_getindex(items, n)),
        )
    )

@inline empty_output(::Type{StaticBitVector{<:Any, U}}) where {U} =
    StaticBitVector{0, U, Tuple{}}(())

@inline non_lazy_iterator(itr::StaticBitVector) = itr

Base.@propagate_inbounds unrolled_map_into(
    ::Type{StaticBitVector{<:Any, U}},
    f::F,
    itr,
) where {U, F} = StaticBitVector{length(itr), U}(
    n -> (Base.@_propagate_inbounds_meta; f(generic_getindex(itr, n))),
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
        last_int = @inbounds itr.ints[n_ints]
        new_last_int =
            last_int & ~(one(U) << bit_offset) | U(bit) << bit_offset
        unrolled_push(unrolled_take(itr.ints, Val(n_ints - 1)), new_last_int)
    end
    return StaticBitVector{length(itr) + 1, U, typeof(ints)}(ints)
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
        mid_int1 = @inbounds itr1.ints[n_ints1]
        mid_int2 = @inbounds itr2.ints[1]
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
    return StaticBitVector{length(itr1) + length(itr2), U, typeof(ints)}(ints)
end

@inline function unrolled_take_into(
    ::Type{StaticBitVector{<:Any, U}},
    itr::StaticBitVector{<:Any, U},
    ::Val{N},
) where {N, U}
    (N < 0 || N > length(itr)) && Base.throw_boundserror(itr, N)
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = _masked_ints(Val(N), U, unrolled_take(itr.ints, Val(n_ints)))
    return StaticBitVector{N, U, typeof(ints)}(ints)
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
            cur_int = @inbounds ints_without_offset[k]
            k == length(ints_without_offset) ? cur_int >> bit_offset :
            cur_int >> bit_offset |
            @inbounds(ints_without_offset[k + 1]) <<
            (n_bits_per_int - bit_offset)
        end
    end
    return StaticBitVector{length(itr) - N, U, typeof(ints)}(ints)
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

Base.@propagate_inbounds function unrolled_accumulate_into(
    ::Type{StaticBitVector{<:Any, U}},
    op::O,
    itr,
    init,
) where {U, O}
    N = length(itr)
    n_bits_per_int = 8 * sizeof(U)
    n_ints = cld(N, n_bits_per_int)
    ints = unrolled_accumulate(
        StaticOneTo(n_ints),
        (nothing, init),
    ) do (_, init_value_for_new_int), int_index
        Base.@_propagate_inbounds_meta
        first_index = n_bits_per_int * (int_index - 1) + 1
        unrolled_reduce(
            StaticOneTo(n_bits_per_int),
            (zero(U), init_value_for_new_int),
        ) do (int, prev_value), bit_index
            Base.@_propagate_inbounds_meta
            bit_offset = bit_index - 1
            index = first_index + bit_offset
            if index <= N
                item = generic_getindex(itr, index)
                new_value =
                    index == 1 && prev_value isa NoInit ?
                    reduction_first(op, item) : op(prev_value, item)
                (int | U(new_value::Bool) << bit_offset, new_value)
            else
                (int, prev_value)
            end
        end
    end
    word_ints = unrolled_map(first, ints)
    return StaticBitVector{N, U, typeof(word_ints)}(word_ints)
end

# `~` also flips the unused bits of the last word, so they are cleared to keep
# the result identical to a vector constructed from the negated bits.
@inline function unrolled_map(
    ::typeof(!),
    itr::StaticBitVector{N, U},
) where {N, U}
    ints = _masked_ints(Val(N), U, unrolled_map(~, itr.ints))
    return StaticBitVector{N, U, typeof(ints)}(ints)
end

@inline unrolled_any(::typeof(identity), itr::StaticBitVector) =
    unrolled_any(!iszero, _masked_ints(itr))
@inline unrolled_any(::typeof(!), itr::StaticBitVector) =
    !unrolled_all(identity, itr)

@inline function unrolled_all(
    ::typeof(identity),
    itr::StaticBitVector{N, U},
) where {N, U}
    N == 0 && return true
    n_bits_per_int = 8 * sizeof(U)
    rem_bits = N % n_bits_per_int
    last_all_ones = rem_bits == 0 ? ~zero(U) : (one(U) << rem_bits) - one(U)
    n_ints = length(itr.ints)
    return unrolled_all(StaticOneTo(n_ints)) do k
        @inline
        word = @inbounds itr.ints[k]
        k == n_ints ? (word & last_all_ones) == last_all_ones : word == ~zero(U)
    end
end
@inline unrolled_all(::typeof(!), itr::StaticBitVector) =
    !unrolled_any(identity, itr)

@inline unrolled_count(::typeof(identity), itr::StaticBitVector) =
    Int(_unrolled_sum(count_ones, _masked_ints(itr), NoInit()))
@inline unrolled_count(::typeof(!), itr::StaticBitVector) =
    length(itr) - unrolled_count(identity, itr)

@inline word_level_reduction(::typeof(&)) = unrolled_all
@inline word_level_reduction(::typeof(|)) = unrolled_any

@inline function _unrolled_bitvector_mapreduce(
    f::Union{typeof(identity), typeof(!)},
    op::Union{typeof(&), typeof(|)},
    itr::StaticBitVector,
    init,
)
    isempty(itr) && return empty_reduction_value(init)
    result = word_level_reduction(op)(f, itr)
    return init isa NoInit ? result : op(init, result)
end

@inline unrolled_reduce(
    op::Union{typeof(&), typeof(|)},
    itr::StaticBitVector,
    init,
) = _unrolled_bitvector_mapreduce(identity, op, itr, init)
@inline unrolled_mapreduce(
    f::Union{typeof(identity), typeof(!)},
    op::Union{typeof(&), typeof(|)},
    itr::StaticBitVector;
    init = NoInit(),
) = _unrolled_bitvector_mapreduce(f, op, itr, init)
