module UnrolledUtilitiesStaticArraysExt

import UnrolledUtilities
import StaticArrays: SVector, MVector

@inline UnrolledUtilities.output_type_for_promotion(::SVector) = SVector
@inline UnrolledUtilities.constructor_from_tuple(::Type{SVector}) = SVector
@inline UnrolledUtilities.non_lazy_iterator(v::SVector) = v

@inline UnrolledUtilities.output_type_for_promotion(::MVector) = MVector
@inline UnrolledUtilities.constructor_from_tuple(::Type{MVector}) = MVector

Base.@propagate_inbounds UnrolledUtilities.unrolled_map(
    f::F,
    v::SVector{N},
) where {F, N} = SVector{N}(UnrolledUtilities.unrolled_map(f, Tuple(v)))
Base.@propagate_inbounds UnrolledUtilities.unrolled_map(
    f::F,
    v1::SVector{N},
    v2::SVector{N},
) where {F, N} =
    SVector{N}(UnrolledUtilities.unrolled_map(f, Tuple(v1), Tuple(v2)))

Base.@propagate_inbounds UnrolledUtilities.unrolled_foreach(
    f::F,
    v::SVector,
) where {F} = UnrolledUtilities.unrolled_foreach(f, Tuple(v))
Base.@propagate_inbounds UnrolledUtilities.unrolled_foreach(
    f::F,
    v1::SVector{N},
    v2::SVector{N},
) where {F, N} = UnrolledUtilities.unrolled_foreach(f, Tuple(v1), Tuple(v2))

@static if hasfield(Method, :recursion_relation)
    for f in (
            UnrolledUtilities.output_type_for_promotion,
            UnrolledUtilities.constructor_from_tuple,
            UnrolledUtilities.non_lazy_iterator,
            UnrolledUtilities.unrolled_map,
            UnrolledUtilities.unrolled_foreach,
        ),
        method in methods(f)

        method.module === (@__MODULE__) || continue
        method.recursion_relation = Returns(true)
    end
end

end
