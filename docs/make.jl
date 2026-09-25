using Documenter
using UnrolledUtilities

comparison_tables_file = joinpath(@__DIR__, "src", "comparison_tables.md")
preamble_file = joinpath(@__DIR__, "src", "comparison_tables_preamble.md")
cp(preamble_file, comparison_tables_file; force = true)

# The titles of the comparison tables in test/test_and_analyze.jl, which are
# also the targets of @ref links in user_guide.md.
comparison_table_titles = (
    "Isolated Unrolled Functions",
    "Nested Unrolled Functions",
    "Recursive Unrolled Functions",
    "Nested Unrolled Closures",
    "Empty Iterators",
    "Very Long Iterators",
    "Manual vs. Recursive Unrolling",
)

# The tables come from the benchmarks in test/test_and_analyze.jl, which take
# about half an hour. Setting UNROLLED_UTILITIES_BENCHMARK=false builds the
# documentation without them, with only the section headings that other pages
# link to.
if get(ENV, "UNROLLED_UTILITIES_BENCHMARK", "true") == "true"
    ENV["UNROLLED_UTILITIES_BENCHMARK"] = "true" # read by test_and_analyze.jl
    include(joinpath(@__DIR__, "..", "test", "test_and_analyze.jl"))
    Tuple(keys(comparison_table_dicts)) == comparison_table_titles ||
        error("comparison_table_titles does not match test_and_analyze.jl")
    open(comparison_tables_file, "a") do io
        for (title, comparison_table_dict) in comparison_table_dicts
            print_comparison_table(title, comparison_table_dict, io)
        end
    end
else
    open(comparison_tables_file, "a") do io
        println(
            io,
            "\n!!! note \"Tables omitted from this build\"\n    This build \
             was made with `UNROLLED_UTILITIES_BENCHMARK=false`, which skips \
             the benchmarks that generate the comparison tables.",
        )
        for title in comparison_table_titles
            println(io, "\n## $title\n")
        end
    end
end

makedocs(;
    sitename = "UnrolledUtilities.jl",
    modules = [UnrolledUtilities],
    checkdocs = :exports,
    pages = [
        "Home" => "index.md",
        "Introduction" => "introduction.md",
        "User Guide" => "user_guide.md",
        "Cookbook" => "cookbook.md",
        "Compilation Limits" => "limits.md",
        "API Reference" => "api.md",
        "Developer Guide" => "developer_guide.md",
        "Comparison Tables" => basename(comparison_tables_file),
    ],
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        sidebar_sitename = false,
        size_threshold_ignore = [
            "api.md",
            "introduction.md",
            basename(comparison_tables_file),
        ],
    ),
    clean = true,
)

rm(comparison_tables_file)

deploydocs(
    repo = "github.com/CliMA/UnrolledUtilities.jl.git",
    target = "build",
    devbranch = "main",
    push_preview = true,
    forcepush = true,
)
