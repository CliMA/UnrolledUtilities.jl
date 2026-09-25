"""
    StaticOneTo{N} <: StaticSequence{N}
    StaticOneTo(N)

Analogue of `Base.OneTo(N)` for the integers `1:N` that stores `N` as a type
parameter.

The items are known during compilation, so unrolled functions pass constant
indices to their callables. Unrolled functions return `Tuple`s for a
`StaticOneTo`, except [`unrolled_take`](@ref), which returns a `StaticOneTo`.

# Examples
```julia
r = StaticOneTo(4)
unrolled_take(r, Val(2)) # StaticOneTo{2}()
unrolled_map(abs2, r)    # (1, 4, 9, 16)
```

See also [`StaticSequence`](@ref), [`StaticBitVector`](@ref).
"""
struct StaticOneTo{N} <: StaticSequence{N} end
@inline StaticOneTo(N) = StaticOneTo{N}()

@inline generic_getindex(::StaticOneTo, n) = n

@inline output_type_for_promotion(::StaticOneTo) = NoOutputType()
@inline eltype_for_promotion(::StaticOneTo) = Int

@inline unrolled_take(r::StaticOneTo, ::Val{N}) where {N} =
    N < 0 || N > length(r) ? Base.throw_boundserror(r, N) : StaticOneTo(N)
