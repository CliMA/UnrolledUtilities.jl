using Test
using JET
using OrderedCollections
using PrettyTables
using InteractiveUtils

using UnrolledUtilities

comparison_table_dicts = OrderedDict()

function print_comparison_table(title, comparison_table_dict, io = stdout)
    table_data =
        mapreduce(vcat, collect(comparison_table_dict)) do (key, entries)
            stack(entry -> (key..., entry...), entries; dims = 1)
        end

    writing_to_docs = io isa IOStream

    color(color_str) =
        writing_to_docs ? HtmlDecoration(; color = color_str) :
        Crayon(; foreground = Symbol(color_str))
    highlighter_color(optimization, run_time, compile_time, allocs) =
        if contains(optimization, "better") ||
           contains(optimization, "fewer allocs") &&
           !contains(run_time, "more") ||
           contains(optimization, "identical") && contains(run_time, "less")
            # better performance
            if !contains(run_time, "more") &&
               !contains(compile_time, "more") &&
               !contains(allocs, "more")
                # similar or better run time, compilation, and total allocations
                if contains(optimization, "better")
                    # better optimization
                    color(writing_to_docs ? "darkturquoise" : "cyan")
                else
                    # faster run time or fewer allocations at run time
                    color(writing_to_docs ? "mediumseagreen" : "green")
                end
            else
                # worse run time, compilation, or total allocations
                if contains(optimization, "better")
                    # better optimization
                    color(writing_to_docs ? "royalblue" : "blue")
                else
                    # faster run time or fewer allocations at run time
                    color(writing_to_docs ? "khaki" : "yellow")
                end
            end
        elseif contains(optimization, "identical") &&
               contains(run_time, "similar")
            # similar performance
            if contains(compile_time, "less") && !contains(allocs, "more") ||
               !contains(compile_time, "more") && contains(allocs, "less")
                # better compilation or total allocations
                color(writing_to_docs ? "mediumorchid" : "magenta")
            elseif contains(compile_time, "less") && contains(allocs, "more") ||
                   contains(compile_time, "more") && contains(allocs, "less")
                # mixed compilation and total allocations
                color(writing_to_docs ? "silver" : "light_gray")
            elseif contains(compile_time, "similar") &&
                   contains(allocs, "similar")
                # similar compilation and total allocations
                color(writing_to_docs ? "gray" : "dark_gray")
            else
                # worse compilation or total allocations
                color(writing_to_docs ? "indianred" : "red")
            end
        else
            # worse performance
            color(writing_to_docs ? "indianred" : "red")
        end
    highlighter = (writing_to_docs ? HtmlHighlighter : Highlighter)(
        Returns(true),
        (_, data, row, _) -> highlighter_color(data[row, 6:9]...),
    )

    # TODO: Why does Sys.maxrss() always seem to be 0 on Ubuntu systems?
    has_rss = any(contains('['), table_data[:, 9])

    other_kwargs =
        writing_to_docs ?
        (;
            backend = Val(:html),
            table_style = Dict(
                "font-family" => "monospace",
                "font-size" => "70%",
            ),
        ) :
        (;
            title,
            title_alignment = :c,
            title_same_width_as_table = true,
            columns_width = [45, 45, 15, 10, 30, 25, 20, 20, has_rss ? 30 : 20],
            linebreaks = true,
            autowrap = true,
            crop = :none,
        )

    if writing_to_docs
        println(io, "## $title")
        println(io, "```@raw html")
        println(io, "<div style=\"width: max(80vw, 100%)\">") # 80% of viewport
    end
    pretty_table(
        io,
        table_data;
        alignment = :l,
        header = [
            "Unrolled Expression",
            "Reference Expression",
            "Itr Type",
            "Itr Length",
            "Itr Contents",
            "Optimization",
            "Run Time",
            "Compilation Time",
            "Total $(has_rss ? "GC [and RSS] " : "")Allocations",
        ],
        highlighters = highlighter,
        other_kwargs...,
    )
    if writing_to_docs
        println(io, "</div>")
        println(io, "```")
    else
        println(io)
    end
end

function time_string(nanoseconds)
    nanoseconds == 0 && return "$nanoseconds ns"
    n_decimal_digits = floor(Int, log10(nanoseconds) + 1)
    return if n_decimal_digits <= 3
        "$nanoseconds ns"
    elseif n_decimal_digits <= 6
        "$(round(Int, nanoseconds / 10^3)) μs"
    elseif n_decimal_digits <= 9
        "$(round(Int, nanoseconds / 10^6)) ms"
    else
        "$(round(Int, nanoseconds / 10^9)) s"
    end
end

function memory_string(bytes)
    bytes == 0 && return "$bytes B"
    n_binary_digits = floor(Int, log2(bytes) + 1)
    return if n_binary_digits <= 10
        "$bytes B"
    elseif n_binary_digits <= 20
        "$(round(Int, bytes / 2^10)) kB"
    elseif n_binary_digits <= 30
        "$(round(Int, bytes / 2^20)) MB"
    else
        "$(round(Int, bytes / 2^30)) GB"
    end
end

function comparison_string(value1, value2, to_string, to_number = identity)
    ratio = to_number(value1) / to_number(value2)
    ratio_str = if ratio >= 2
        floored_ratio = ratio == Inf ? Inf : floor(Int, ratio)
        "$floored_ratio times more"
    elseif inv(ratio) >= 2
        floored_inv_ratio = ratio == 0 ? Inf : floor(Int, inv(ratio))
        "$floored_inv_ratio times less"
    else
        "similar"
    end
    return "$ratio_str ($(to_string(value1)) vs. $(to_string(value2)))"
end

function drop_line_numbers(expr)
    expr isa Expr || return expr
    new_args = map(drop_line_numbers, expr.args)
    expr.head == :block || return Expr(expr.head, new_args...)
    filtered_args = filter(arg -> !(arg isa LineNumberNode), new_args)
    return length(filtered_args) == 1 ? filtered_args[1] :
           Expr(expr.head, filtered_args...)
end

simplified_expression_string(expr) =
    replace(string(drop_line_numbers(expr)), r"#=.+=# @" => '@', r"\s+" => ' ')

function code_instance(f, args...)
    available_methods = methods(f, Tuple{map(typeof, args)...})
    @assert length(available_methods) == 1
    (; specializations) = available_methods[1]
    specTypes = Tuple{typeof(f), map(typeof, args)...}
    return if specializations isa Core.MethodInstance
        @assert specializations.specTypes == specTypes
        specializations.cache
    else
        matching_specialization_indices =
            findall(specializations) do specialization
                !isnothing(specialization) &&
                    specialization.specTypes == specTypes
            end
        @assert length(matching_specialization_indices) == 1
        specializations[matching_specialization_indices[1]].cache
    end
end

macro benchmark(expression)
    return quote
        prev_time = time_ns()
        $(esc(expression))
        new_time = time_ns()
        best_time = new_time - prev_time

        # Benchmark for at most 0.1 s (10^8 ns), ignoring the first call above.
        n_trials = 0
        start_time = new_time
        while n_trials < 10^4 && new_time - start_time < 10^8
            prev_time = time_ns()
            $(esc(expression))
            new_time = time_ns()
            best_time = min(best_time, new_time - prev_time)
            n_trials += 1
        end

        best_time
    end
end

macro test_unrolled(
    args_expr,
    unrolled_expr,
    reference_expr,
    itr_contents_str,
    skip_allocations_test = false,
    skip_type_stability_test = false,
)
    @assert Meta.isexpr(args_expr, :tuple)
    arg_names = args_expr.args
    @assert all(arg_name -> arg_name isa Symbol, arg_names)
    args = map(esc, arg_names)
    unrolled_expr_str = simplified_expression_string(unrolled_expr)
    reference_expr_str = simplified_expression_string(reference_expr)
    contains_str = length(args) == 1 ? " that contains" : "s that each contain"
    quote
        itr_types = map(arg -> typeof(arg).name.wrapper, ($(args...),))
        itr_lengths = map(length, ($(args...),))

        itr_type_str =
            length(unique(itr_types)) == 1 ? string(itr_types[1]) :
            join(itr_types, '/')
        itr_length_str =
            length(unique(itr_lengths)) == 1 ? string(itr_lengths[1]) :
            join(itr_lengths, '/')
        itr_str =
            $(isempty(args)) ? "nothing" :
            "$($(length(args))) $itr_type_str$($contains_str) $itr_length_str \
             $($(esc(itr_contents_str)))"

        @info "Testing $($unrolled_expr_str) with $itr_str"

        unrolled_func($(arg_names...)) = $(esc(unrolled_expr))
        reference_func($(arg_names...)) = $(esc(reference_expr))

        # Test for correctness.
        @test unrolled_func($(args...)) == reference_func($(args...))

        unrolled_func_and_nothing($(arg_names...)) =
            ($(esc(unrolled_expr)); nothing)
        reference_func_and_nothing($(arg_names...)) =
            ($(esc(reference_expr)); nothing)

        unrolled_func_and_nothing($(args...)) # Run once to compile.
        reference_func_and_nothing($(args...))

        # Test for allocations.
        unrolled_run_memory = @allocated unrolled_func_and_nothing($(args...))
        reference_run_memory = @allocated reference_func_and_nothing($(args...))
        $(esc(skip_allocations_test)) || @test unrolled_run_memory == 0

        # Test for type-stability.
        is_unrolled_stable =
            isempty(JET.get_reports(@report_opt unrolled_func($(args...))))
        is_reference_stable =
            isempty(JET.get_reports(@report_opt reference_func($(args...))))
        $(esc(skip_type_stability_test)) || @test_opt unrolled_func($(args...))

        # Test for constant propagation.
        is_unrolled_const =
            isdefined(code_instance(unrolled_func, $(args...)), :rettype_const)
        is_reference_const =
            isdefined(code_instance(reference_func, $(args...)), :rettype_const)
        # Base.issingletontype(typeof(($(args...),))) && @test is_unrolled_const

        buffer = IOBuffer()

        # Determine whether the functions are fully optimized out.
        args_type = Tuple{map(typeof, ($(args...),))...}
        code_llvm(buffer, unrolled_func, args_type; debuginfo = :none)
        is_unrolled_optimized_out =
            length(split(String(take!(buffer)), '\n')) == 5
        code_llvm(buffer, reference_func, args_type; debuginfo = :none)
        is_reference_optimized_out =
            length(split(String(take!(buffer)), '\n')) == 5

        # Test the overall level of optimization.
        unrolled_opt_str, unrolled_opt_score = if unrolled_run_memory > 0
            "$(memory_string(unrolled_run_memory)) allocs", 1 / unrolled_run_memory
        elseif !is_unrolled_stable
            "type-unstable", 2
        elseif !is_unrolled_const && !is_unrolled_optimized_out
            "type-stable", 3
        elseif !is_unrolled_optimized_out
            "constant", 4
        else
            "optimized out", 5
        end
        reference_opt_str, reference_opt_score = if reference_run_memory > 0
            "$(memory_string(reference_run_memory)) allocs",
            1 / reference_run_memory
        elseif !is_reference_stable
            "type-unstable", 2
        elseif !is_reference_const && !is_reference_optimized_out
            "type-stable", 3
        elseif !is_reference_optimized_out
            "constant", 4
        else
            "optimized out", 5
        end
        $(esc(skip_type_stability_test)) ||
            @test unrolled_opt_score >= reference_opt_score

        # Measure the run times.
        unrolled_run_time = @benchmark unrolled_func($(args...))
        reference_run_time = @benchmark reference_func($(args...))

        # Measure the compilation times and memory allocations in separate
        # processes to ensure that they are not under-counted.
        arg_name_strs = ($(map(string, arg_names)...),)
        arg_names_str = join(arg_name_strs, ", ")
        arg_definition_strs =
            map((name, value) -> "$name = $value", arg_name_strs, ($(args...),))
        arg_definitions_str = join(arg_definition_strs, '\n')
        command_str(func_str) = """
            using UnrolledUtilities
            $arg_definitions_str
            Base.cumulative_compile_timing(true)
            nanoseconds1 = Base.cumulative_compile_time_ns()[1]
            rss_bytes_1 = Sys.maxrss()
            Δgc_bytes = @allocated $func_str
            rss_bytes_2 = Sys.maxrss()
            nanoseconds2 = Base.cumulative_compile_time_ns()[1]
            Base.cumulative_compile_timing(false)
            Δnanoseconds = nanoseconds2 - nanoseconds1
            Δrss_bytes = rss_bytes_2 - rss_bytes_1
            print(Δnanoseconds, ", ", Δgc_bytes, ", ", Δrss_bytes)
            """

        unrolled_command_str = command_str($(string(unrolled_expr)))
        run(pipeline(`julia --project -e $unrolled_command_str`, buffer))
        unrolled_compile_time, unrolled_total_memory, unrolled_total_rss =
            parse.((Int, Int, Int), split(String(take!(buffer)), ','))

        # Make a new buffer to avoid a potential data race:
        # discourse.julialang.org/t/iobuffer-becomes-not-writable-after-run/92323/3
        close(buffer)
        buffer = IOBuffer()

        reference_command_str = command_str($(string(reference_expr)))
        run(pipeline(`julia --project -e $reference_command_str`, buffer))
        reference_compile_time, reference_total_memory, reference_total_rss =
            parse.((Int, Int, Int), split(String(take!(buffer)), ','))

        close(buffer)

        optimization_str = if unrolled_opt_score > reference_opt_score
            if unrolled_opt_score <= 1
                "fewer allocs ($unrolled_opt_str vs. $reference_opt_str)"
            else
                "better ($unrolled_opt_str vs. $reference_opt_str)"
            end
        elseif unrolled_opt_score < reference_opt_score
            "worse ($unrolled_opt_str vs. $reference_opt_str)"
        else
            "identical ($unrolled_opt_str)"
        end
        run_time_str = comparison_string(
            unrolled_run_time,
            reference_run_time,
            time_string,
        )
        compile_time_str = comparison_string(
            unrolled_compile_time,
            reference_compile_time,
            time_string,
        )
        memory_str = comparison_string(
            (unrolled_total_memory, unrolled_total_rss),
            (reference_total_memory, reference_total_rss),
            ((gc_bytes, rss_bytes),) ->
                rss_bytes == 0 ? memory_string(gc_bytes) :
                "$(memory_string(gc_bytes)) [$(memory_string(rss_bytes))]",
            first, # Use GC value for comparison since RSS might be unavailable.
        )

        dict_key = ($unrolled_expr_str, $reference_expr_str)
        dict_entry = (
            itr_type_str,
            itr_length_str,
            $(esc(itr_contents_str)),
            optimization_str,
            run_time_str,
            compile_time_str,
            memory_str,
        )
        if dict_key in keys(comparison_table_dict)
            push!(comparison_table_dict[dict_key], dict_entry)
        else
            comparison_table_dict[dict_key] = [dict_entry]
        end
    end
end

tuple_of_tuples(num_tuples, min_tuple_length, singleton, identical) =
    ntuple(num_tuples) do index
        tuple_length = min_tuple_length + (identical ? 0 : (index - 1) % 7)
        ntuple(singleton ? Val : identity, tuple_length)
    end
function tuples_of_tuples_contents_str(itrs...)
    str = ""
    all(itr -> length(itr) > 1 && length(unique(itr)) == 1, itrs) &&
        (str *= "identical ")
    all(itr -> length(itr) > 1 && length(unique(itr)) != 1, itrs) &&
        (str *= "distinct ")
    all(itr -> all(isempty, itr), itrs) && (str *= "empty ")
    all(itr -> all(!isempty, itr), itrs) && (str *= "nonempty ")
    all(itr -> any(isempty, itr) && any(!isempty, itr), itrs) &&
        (str *= "empty & nonempty ")
    all(itr -> Base.issingletontype(typeof(itr)), itrs) && (str *= "singleton ")
    all(itr -> !Base.issingletontype(typeof(itr)), itrs) &&
        (str *= "non-singleton ")
    str *= "Tuple"
    all(itr -> length(itr) > 1, itrs) && (str *= "s")
    return str
end

# NOTE: In the tests below, random numbers are meant to emulate values that
# cannot be inferred during compilation.

title = "Isolated Unrolled Functions"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

for itr in (
    tuple_of_tuples(1, 0, true, true),
    tuple_of_tuples(1, 1, true, true),
    tuple_of_tuples(1, 1, false, true),
    map(n -> tuple_of_tuples(n, 0, true, true), (8, 32, 33, 128))...,
    map(n -> tuple_of_tuples(n, 1, true, true), (8, 32, 33, 128))...,
    map(n -> tuple_of_tuples(n, 1, false, true), (8, 32, 33, 128))...,
    map(n -> tuple_of_tuples(n, 0, true, false), (8, 32, 33, 128))...,
    map(n -> tuple_of_tuples(n, 1, true, false), (8, 32, 33, 128))...,
    map(n -> tuple_of_tuples(n, 1, false, false), (8, 32, 33, 128))...,
)
    str = tuples_of_tuples_contents_str(itr)
    itr_description = "a Tuple that contains $(length(itr)) $str"
    @testset "individual unrolled functions of $itr_description" begin
        @test_unrolled (itr,) unrolled_any(isempty, itr) any(isempty, itr) str
        @test_unrolled(
            (itr,),
            unrolled_any(x -> length(x) == rand(8:10), itr),
            any(x -> length(x) == rand(8:10), itr),
            str,
        )

        @test_unrolled (itr,) unrolled_all(isempty, itr) all(isempty, itr) str
        @test_unrolled(
            (itr,),
            unrolled_all(x -> length(x) == rand(8:10), itr),
            all(x -> length(x) == rand(8:10), itr),
            str,
        )

        @test_unrolled(
            (itr,),
            unrolled_foreach(x -> @assert(length(x) <= 7), itr),
            foreach(x -> @assert(length(x) <= 7), itr),
            str,
        )

        @test_unrolled (itr,) unrolled_map(length, itr) map(length, itr) str

        @test_unrolled(
            (itr,),
            unrolled_applyat(length, rand(1:7:length(itr)), itr),
            length(itr[rand(1:7:length(itr))]),
            str,
        )

        @test_unrolled (itr,) unrolled_reduce(tuple, itr) reduce(tuple, itr) str
        @test_unrolled(
            (itr,),
            unrolled_reduce(tuple, itr; init = ()),
            reduce(tuple, itr; init = ()),
            str,
        )

        @test_unrolled(
            (itr,),
            unrolled_mapreduce(length, +, itr),
            mapreduce(length, +, itr),
            str,
        )
        @test_unrolled(
            (itr,),
            unrolled_mapreduce(length, +, itr; init = 0),
            mapreduce(length, +, itr; init = 0),
            str,
        )

        if length(itr) <= 33
            @test_unrolled(
                (itr,),
                unrolled_accumulate(tuple, itr),
                accumulate(tuple, itr),
                str,
            )
            @test_unrolled(
                (itr,),
                unrolled_accumulate(tuple, itr; init = ()),
                accumulate(tuple, itr; init = ()),
                str,
            )
        end # These can take half a minute to compile when the length is 128.

        @test_unrolled (itr,) unrolled_push(itr, itr[1]) (itr..., itr[1]) str
        @test_unrolled (itr,) unrolled_append(itr, itr) (itr..., itr...) str

        @test_unrolled(
            (itr,),
            unrolled_take(itr, Val(length(itr) ÷ 2)),
            itr[1:(length(itr) ÷ 2)],
            str,
        )
        @test_unrolled(
            (itr,),
            unrolled_drop(itr, Val(length(itr) ÷ 2)),
            itr[(length(itr) ÷ 2 + 1):end],
            str,
        )

        @test_unrolled (itr,) unrolled_in(nothing, itr) (nothing in itr) str
        @test_unrolled (itr,) unrolled_in(itr[1], itr) (itr[1] in itr) str
        @test_unrolled (itr,) unrolled_in(itr[end], itr) (itr[end] in itr) str

        @test_unrolled(
            (itr,),
            unrolled_unique(itr),
            Tuple(unique(itr)),
            str,
            !Base.issingletontype(typeof(itr)),
            !Base.issingletontype(typeof(itr)),
        ) # unrolled_unique is type-unstable for non-singleton values

        @test_unrolled(
            (itr,),
            unrolled_filter(!isempty, itr),
            filter(!isempty, itr),
            str,
        )

        @test_unrolled(
            (itr,),
            unrolled_split(isempty, itr),
            (filter(isempty, itr), filter(!isempty, itr)),
            str,
        )

        @test_unrolled(
            (itr,),
            unrolled_flatten(itr),
            Tuple(Iterators.flatten(itr)),
            str,
        )

        @test_unrolled(
            (itr,),
            unrolled_flatmap(reverse, itr),
            Tuple(Iterators.flatmap(reverse, itr)),
            str,
        )

        if length(itr) <= 33
            @test_unrolled(
                (itr,),
                unrolled_product(itr, itr),
                Tuple(Iterators.product(itr, itr)),
                str,
            )
        end
        if length(itr) <= 8
            @test_unrolled(
                (itr,),
                unrolled_product(itr, itr, itr),
                Tuple(Iterators.product(itr, itr, itr)),
                str,
            )
        end # This can take several minutes to compile when the length is 32.
    end
end

title = "Nested Unrolled Functions"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

for (itr1, itr2, itr3) in (
    (
        tuple_of_tuples(1, 0, true, true),
        tuple_of_tuples(1, 1, true, true),
        tuple_of_tuples(1, 1, false, true),
    ),
    zip(
        map(n -> tuple_of_tuples(n, 0, true, true), (8, 32, 33, 128)),
        map(n -> tuple_of_tuples(n, 1, true, true), (8, 32, 33, 128)),
        map(n -> tuple_of_tuples(n, 1, false, true), (8, 32, 33, 128)),
    )...,
    zip(
        map(n -> tuple_of_tuples(n, 0, true, false), (8, 32, 33, 128)),
        map(n -> tuple_of_tuples(n, 1, true, false), (8, 32, 33, 128)),
        map(n -> tuple_of_tuples(n, 1, false, false), (8, 32, 33, 128)),
    )...,
)
    str3 = tuples_of_tuples_contents_str(itr3)
    str12 = tuples_of_tuples_contents_str(itr1, itr2)
    str23 = tuples_of_tuples_contents_str(itr2, itr3)
    str123 = tuples_of_tuples_contents_str(itr1, itr2, itr3)
    itr_description = "Tuples that contain $(length(itr1)) $str123"
    @testset "nested unrolled functions of $itr_description" begin
        @test_unrolled(
            (itr3,),
            unrolled_any(x -> unrolled_reduce(+, x) > 7, itr3),
            any(x -> reduce(+, x) > 7, itr3),
            str3,
        )

        @test_unrolled(
            (itr3,),
            unrolled_mapreduce(x -> unrolled_reduce(+, x), max, itr3),
            mapreduce(x -> reduce(+, x), max, itr3),
            str3,
        )

        @test_unrolled(
            (itr1, itr2),
            unrolled_foreach(
                (x1, x2) -> @assert(x1 == unrolled_take(x2, Val(length(x1)))),
                itr1,
                itr2,
            ),
            foreach((x1, x2) -> @assert(x1 == x2[1:length(x1)]), itr1, itr2),
            str12,
        )
        @test_unrolled(
            (itr2, itr3),
            unrolled_foreach(
                (x2, x3) -> @assert(x2 == unrolled_map(Val, x3)),
                itr2,
                itr3,
            ),
            foreach((x2, x3) -> @assert(x2 == map(Val, x3)), itr2, itr3),
            str23,
        )

        @test_unrolled(
            (itr1, itr2),
            unrolled_applyat(
                (x1, x2) -> @assert(x1 == unrolled_take(x2, Val(length(x1)))),
                rand(1:length(itr1)),
                itr1,
                itr2,
            ),
            let n = rand(1:length(itr1))
                @assert(itr1[n] == itr2[n][1:length(itr1[n])])
            end,
            str12,
        )
        @test_unrolled(
            (itr2, itr3),
            unrolled_applyat(
                (x2, x3) -> @assert(x2 == unrolled_map(Val, x3)),
                rand(1:length(itr2)),
                itr2,
                itr3,
            ),
            let n = rand(1:length(itr2))
                @assert(itr2[n] == map(Val, itr3[n]))
            end,
            str23,
        )
    end
end

nested_iterator(depth, n, inner_n) =
    depth == 1 ? ntuple(identity, n) :
    ntuple(
        Returns(nested_iterator(depth - 1, Int(n / inner_n), inner_n)),
        inner_n,
    )

title = "Recursive Unrolled Functions"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

for n in (8, 32, 128)
    itr_description = "a Tuple that contains $n values in nested Tuples"
    @testset "recursive unrolled functions of $itr_description" begin
        for depth in (2, 3, 4:2:(Int(log2(n)) + 1)...)
            itr = nested_iterator(depth, n, 2)
            str = "$itr_description of depth $depth"
            # In the following definitions, use var"#self#" to avoid boxing:
            # discourse.julialang.org/t/performant-recursive-anonymous-functions/90984/5
            @test_unrolled(
                (itr,),
                map(
                    x ->
                        eltype(x) <: Tuple ?
                        unrolled_mapreduce(var"#self#", +, x) : length(x),
                    (itr,),
                )[1],
                map(
                    x ->
                        eltype(x) <: Tuple ? mapreduce(var"#self#", +, x) :
                        length(x),
                    (itr,),
                )[1],
                str,
            ) # nested iterator length
            @test_unrolled(
                (itr,),
                map(
                    x ->
                        eltype(x) <: Tuple ?
                        unrolled_mapreduce(var"#self#", +, x) :
                        unrolled_reduce(+, x),
                    (itr,),
                )[1],
                map(
                    x ->
                        eltype(x) <: Tuple ? mapreduce(var"#self#", +, x) :
                        reduce(+, x),
                    (itr,),
                )[1],
                str,
            ) # nested iterator sum
        end
    end
end

title = "Nested Unrolled Closures"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

@testset "nested unrolled closures of Tuples vs. StaticBitVectors" begin
    for (itr, skip_allocations_test) in (
        (ntuple(Returns(true), 32), false),
        (ntuple(Returns(true), 33), true),
        (StaticBitVector{256}(true), false),
        (StaticBitVector{257}(true), true),
    )
        @test_unrolled(
            (itr,),
            unrolled_reduce(
                (itr′, i) -> Base.setindex(itr′, !itr′[i], i),
                StaticOneTo(length(itr));
                init = itr,
            ),
            reduce(
                (itr′, i) -> Base.setindex(itr′, !itr′[i], i),
                StaticOneTo(length(itr));
                init = itr,
            ),
            "Bools",
            skip_allocations_test,
        )
        @test_unrolled(
            (itr,),
            unrolled_reduce(
                (itr′, i) -> unrolled_reduce(
                    (itr′′, j) ->
                        Base.setindex(itr′′, !itr′′[min(i, j)], j),
                    StaticOneTo(length(itr′));
                    init = itr′,
                ),
                StaticOneTo(length(itr));
                init = itr,
            ),
            reduce(
                (itr′, i) -> reduce(
                    (itr′′, j) ->
                        Base.setindex(itr′′, !itr′′[min(i, j)], j),
                    StaticOneTo(length(itr′));
                    init = itr′,
                ),
                StaticOneTo(length(itr));
                init = itr,
            ),
            "Bools",
            skip_allocations_test,
        )
        if length(itr) <= 256
            @test_unrolled(
                (itr,),
                unrolled_reduce(
                    (itr′, i) -> unrolled_reduce(
                        (itr′′, j) -> unrolled_reduce(
                            (itr′′′, k) -> Base.setindex(
                                itr′′′,
                                !itr′′′[min(i, j, k)],
                                k,
                            ),
                            StaticOneTo(length(itr′′));
                            init = itr′′,
                        ),
                        StaticOneTo(length(itr′));
                        init = itr′,
                    ),
                    StaticOneTo(length(itr));
                    init = itr,
                ),
                reduce(
                    (itr′, i) -> reduce(
                        (itr′′, j) -> reduce(
                            (itr′′′, k) -> Base.setindex(
                                itr′′′,
                                !itr′′′[min(i, j, k)],
                                k,
                            ),
                            StaticOneTo(length(itr′′));
                            init = itr′′,
                        ),
                        StaticOneTo(length(itr′));
                        init = itr′,
                    ),
                    StaticOneTo(length(itr));
                    init = itr,
                ),
                "Bools",
                skip_allocations_test,
            )
        end # The StaticBitVector{257} allocates over 2 GB for this test.
    end
end

title = "Empty Iterators"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

@testset "unrolled functions of an empty Tuple" begin
    itr = ()
    str = "nothing"
    @test_unrolled (itr,) unrolled_any(error, itr) any(error, itr) str
    @test_unrolled (itr,) unrolled_all(error, itr) all(error, itr) str
    @test_unrolled (itr,) unrolled_foreach(error, itr) foreach(error, itr) str
    @test_unrolled (itr,) unrolled_map(error, itr) map(error, itr) str
    @test_throws "init" unrolled_reduce(error, itr)
    @test_unrolled(
        (itr,),
        unrolled_reduce(error, itr; init = 0),
        reduce(error, itr; init = 0),
        str,
    )
    @test_unrolled(
        (itr,),
        unrolled_accumulate(error, itr),
        accumulate(error, itr),
        str,
    )
    @test_unrolled(
        (itr,),
        unrolled_accumulate(error, itr; init = 0),
        accumulate(error, itr; init = 0),
        str,
    )
end

title = "Very Long Iterators"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

@testset "unrolled functions of Tuples vs. StaticOneTos" begin
    for itr in (ntuple(identity, 2000), StaticOneTo(2000), StaticOneTo(9000))
        @test_unrolled (itr,) unrolled_reduce(+, itr) reduce(+, itr) "Ints"
        @test_unrolled(
            (itr,),
            unrolled_mapreduce(log, +, itr),
            mapreduce(log, +, itr),
            "Ints",
        )
    end # These can take over a minute to compile for ntuple(identity, 9000).
end

title = "Generative vs. Recursive Unrolling"
comparison_table_dict = (comparison_table_dicts[title] = OrderedDict())

for itr in (
    tuple_of_tuples(1, 0, true, true),
    tuple_of_tuples(1, 1, true, true),
    tuple_of_tuples(1, 1, false, true),
    map(n -> tuple_of_tuples(n, 0, true, true), (8, 16, 32, 33, 128, 256))...,
    map(n -> tuple_of_tuples(n, 1, true, true), (8, 16, 32, 33, 128, 256))...,
    map(n -> tuple_of_tuples(n, 1, false, true), (8, 16, 32, 33, 128, 256))...,
    map(n -> tuple_of_tuples(n, 0, true, false), (8, 16, 32, 33, 128, 256))...,
    map(n -> tuple_of_tuples(n, 1, true, false), (8, 16, 32, 33, 128, 256))...,
    map(n -> tuple_of_tuples(n, 1, false, false), (8, 16, 32, 33, 128, 256))...,
)
    str = tuples_of_tuples_contents_str(itr)
    itr_description = "a Tuple that contains $(length(itr)) $str"
    @testset "generative vs. recursive unrolling of $itr_description" begin
        @test_unrolled(
            (itr,),
            UnrolledUtilities.gen_unrolled_any(isempty, itr),
            UnrolledUtilities.rec_unrolled_any(isempty, itr),
            str,
        )

        @test_unrolled(
            (itr,),
            UnrolledUtilities.gen_unrolled_all(isempty, itr),
            UnrolledUtilities.rec_unrolled_all(isempty, itr),
            str,
        )

        @test_unrolled(
            (itr,),
            UnrolledUtilities.gen_unrolled_foreach(
                x -> @assert(length(x) <= 7),
                itr,
            ),
            UnrolledUtilities.rec_unrolled_foreach(
                x -> @assert(length(x) <= 7),
                itr,
            ),
            str,
        )

        @test_unrolled(
            (itr,),
            UnrolledUtilities.gen_unrolled_map(length, itr),
            UnrolledUtilities.rec_unrolled_map(length, itr),
            str,
        )

        @test_unrolled(
            (itr,),
            UnrolledUtilities.gen_unrolled_applyat(
                length,
                rand(1:7:length(itr)),
                itr,
            ),
            UnrolledUtilities.rec_unrolled_applyat(
                length,
                rand(1:7:length(itr)),
                itr,
            ),
            str,
        )

        if length(itr) <= 33
            @test_unrolled(
                (itr,),
                UnrolledUtilities.gen_unrolled_reduce(tuple, itr, ()),
                UnrolledUtilities.rec_unrolled_reduce(tuple, itr, ()),
                str,
            )

            @test_unrolled(
                (itr,),
                UnrolledUtilities.gen_unrolled_accumulate(
                    tuple,
                    itr,
                    (),
                    identity,
                ),
                UnrolledUtilities.rec_unrolled_accumulate(
                    tuple,
                    itr,
                    (),
                    identity,
                ),
                str,
            )
        end # These can take over a minute to compile when the length is 128.
    end
end
