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

# The benchmarks take about half an hour, so pull request previews contain only
# the section headings; the deployed documentation contains the full tables.
if get(ENV, "UNROLLED_UTILITIES_BENCHMARK", "false") == "true"
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
            "\n!!! note \"Tables omitted from this build\"\n    The comparison \
             tables are generated when the documentation is built with the \
             environment variable `UNROLLED_UTILITIES_BENCHMARK` set to `true`, \
             as it is for the deployed documentation.",
        )
        for title in comparison_table_titles
            println(io, "\n## $title\n")
        end
    end
end

makedocs(;
    sitename = "UnrolledUtilities.jl",
    modules = [UnrolledUtilities],
    pages = [
        "Home" => "index.md",
        "Introduction" => "introduction.md",
        "User Guide" => "user_guide.md",
        "Developer Guide" => "developer_guide.md",
        "Comparison Tables" => basename(comparison_tables_file),
    ],
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        sidebar_sitename = false,
        size_threshold_ignore = [
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
