using Documenter

include(joinpath("..", "test", "test_and_analyze.jl"))

comparison_table_file = joinpath("docs", "src", "comparison_table.md")

open(comparison_table_file, "w") do io
    println(io, "# Comparison Table\n```@raw html")
    println(io, "<div style=\"width: max(80vw, 100%)\">") # use 80% of viewport
    print_comparison_table(io, true)
    println(io, "</div>")
    println(io, "```")
end

makedocs(;
    sitename = "UnrolledUtilities.jl",
    modules = [UnrolledUtilities],
    pages = ["Home" => "index.md", "Comparison Table" => "comparison_table.md"],
    format = Documenter.HTML(
        prettyurls = get(ENV, "CI", nothing) == "true",
        size_threshold_ignore = ["comparison_table.md"],
    ),
    clean = true,
)

rm(comparison_table_file)

deploydocs(
    repo = "github.com/CliMA/UnrolledUtilities.jl.git",
    target = "build",
    devbranch = "main",
    push_preview = true,
    forcepush = true,
)
