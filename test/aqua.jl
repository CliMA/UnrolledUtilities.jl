using Test
import Aqua, UnrolledUtilities

# This is separate from all the other tests because Aqua.test_all checks for
# ambiguities in Base and Core in addition to the ones in UnrolledUtilities,
# resulting in a lot of false positives. Setting recursive to true ensures that
# sub-modules of UnrolledUtilities are also checked for ambiguities.
@testset "Method ambiguity" begin
    Aqua.test_ambiguities(UnrolledUtilities; recursive = true)
end

# Run the other Aqua tests, which are already wrapped in @testset blocks.
Aqua.test_all(UnrolledUtilities; ambiguities = false)
