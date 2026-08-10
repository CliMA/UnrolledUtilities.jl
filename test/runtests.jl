using SafeTestsets

@safetestset "Container and callable regression tests" begin
    @time include("container_and_callable.jl")
end
@safetestset "Test and Analyze" begin
    @time include("test_and_analyze.jl")
    for (title, comparison_table_dict) in comparison_table_dicts
        print_comparison_table(title, comparison_table_dict)
    end
end
@safetestset "Aqua" begin
    @time include("aqua.jl")
end
