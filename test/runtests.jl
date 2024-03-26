using SafeTestsets

#! format: off
@safetestset "Test and Analyze" begin @time include("test_and_analyze.jl") end
@safetestset "Aqua" begin @time include("aqua.jl") end
#! format: on
