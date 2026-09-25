module UnrolledUtilitiesStaticArraysExt

import UnrolledUtilities
import StaticArrays: SVector, MVector

@inline UnrolledUtilities.output_type_for_promotion(::SVector) = SVector
@inline UnrolledUtilities.constructor_from_tuple(::Type{SVector}) = SVector
@inline UnrolledUtilities.non_lazy_iterator(v::SVector) = v

@inline UnrolledUtilities.output_type_for_promotion(::MVector) = MVector
@inline UnrolledUtilities.constructor_from_tuple(::Type{MVector}) = MVector

@static if hasfield(Method, :recursion_relation)
    for f in (
            UnrolledUtilities.output_type_for_promotion,
            UnrolledUtilities.constructor_from_tuple,
            UnrolledUtilities.non_lazy_iterator,
        ),
        method in methods(f)

        method.module === (@__MODULE__) || continue
        method.recursion_relation = Returns(true)
    end
end

end
