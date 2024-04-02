using SafeTestsets

@safetestset "Test and Analyze" begin
    @time include("test_and_analyze.jl")
    print_comparison_table()
end
@safetestset "Aqua" begin
    @time include("aqua.jl")
end
