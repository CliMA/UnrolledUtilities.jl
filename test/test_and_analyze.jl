using Test
using JET
using OrderedCollections
using PrettyTables
using InteractiveUtils

using UnrolledUtilities

comparison_table_dict = OrderedDict()

function print_comparison_table(io = stdout, generate_html = false)
    table_data =
        mapreduce(vcat, collect(comparison_table_dict)) do (key, entries)
            stack(entry -> (key..., entry...), entries; dims = 1)
        end

    highlighter(f, color) =
        generate_html ? HtmlHighlighter(f, HtmlDecoration(; color)) :
        Highlighter(f, Crayon(; foreground = Symbol(color)))

    better_performance_but_harder_to_compile =
        highlighter(generate_html ? "royalblue" : "blue") do data, i, j
            data[i, 4] != data[i, 5] &&
                (endswith(data[i, 6], "slower") || endswith(data[i, 7], "more"))
        end
    better_performance =
        highlighter(generate_html ? "mediumseagreen" : "green") do data, i, j
            data[i, 4] != data[i, 5]
        end
    mixed_compilation =
        highlighter(generate_html ? "mediumorchid" : "magenta") do data, i, j
            (endswith(data[i, 6], "slower") && endswith(data[i, 7], "less")) ||
                (endswith(data[i, 6], "faster") && endswith(data[i, 7], "more"))
        end
    harder_to_compile =
        highlighter(generate_html ? "indianred" : "red") do data, i, j
            endswith(data[i, 6], "slower") || endswith(data[i, 7], "more")
        end
    easier_to_compile =
        highlighter(generate_html ? "darkturquoise" : "cyan") do data, i, j
            endswith(data[i, 6], "faster") || endswith(data[i, 7], "less")
        end
    no_difference =
        highlighter((data, i, j) -> true, generate_html ? "khaki" : "yellow")

    other_kwargs =
        generate_html ?
        (;
            backend = Val(:html),
            table_style = Dict(
                "font-family" => "monospace",
                "font-size" => "70%",
            ),
        ) :
        (;
            title_same_width_as_table = true,
            columns_width = [45, 45, 0, 0, 0, 0, 0],
            linebreaks = true,
            autowrap = true,
            crop = :none,
        )

    pretty_table(
        io,
        table_data;
        title = "Comparison of UnrolledUtilities to Base and Base.Iterators",
        title_alignment = :c,
        alignment = :l,
        header = [
            "Unrolled Expression",
            "Reference Expression",
            "Iterator Contents",
            "Unrolled Performance",
            "Reference Performance",
            "Unrolled Compilation Time",
            "Unrolled Compilation Memory",
        ],
        highlighters = (
            better_performance_but_harder_to_compile,
            better_performance,
            mixed_compilation,
            harder_to_compile,
            easier_to_compile,
            no_difference,
        ),
        other_kwargs...,
    )
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

macro test_unrolled(args_expr, unrolled_expr, reference_expr, contents_info_str)
    @assert Meta.isexpr(args_expr, :tuple)
    arg_names = args_expr.args
    @assert all(arg_name -> arg_name isa Symbol, arg_names)
    args = map(esc, arg_names)
    unrolled_expr_str = simplified_expression_string(unrolled_expr)
    reference_expr_str = simplified_expression_string(reference_expr)
    expr_info_str =
        length(args) == 1 ? "$unrolled_expr_str with 1 iterator that contains" :
        "$unrolled_expr_str with $(length(args)) iterators that each contain"
    quote
        @info "Testing $($expr_info_str) $($(esc(contents_info_str)))"

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
        @test (@allocated unrolled_func_and_nothing($(args...))) == 0
        is_reference_non_allocating =
            (@allocated reference_func_and_nothing($(args...))) == 0

        # Test for type-stability.
        @test_opt unrolled_func($(args...))
        is_reference_stable =
            isempty(JET.get_reports(@report_opt reference_func($(args...))))

        unrolled_instance = code_instance(unrolled_func, $(args...))
        reference_instance = code_instance(reference_func, $(args...))

        # Test for constant propagation.
        is_unrolled_const = isdefined(unrolled_instance, :rettype_const)
        Base.issingletontype(typeof(($(args...),))) && @test is_unrolled_const
        is_reference_const = isdefined(reference_instance, :rettype_const)

        buffer = IOBuffer()

        # Check whether the functions are fully optimized out.
        args_type = Tuple{map(typeof, ($(args...),))...}
        code_llvm(buffer, unrolled_func, args_type; debuginfo = :none)
        is_unrolled_optimized_out =
            length(split(String(take!(buffer)), '\n')) == 5
        code_llvm(buffer, reference_func, args_type; debuginfo = :none)
        is_reference_optimized_out =
            length(split(String(take!(buffer)), '\n')) == 5

        arg_name_strs = ($(map(string, arg_names)...),)
        arg_names_str = join(arg_name_strs, ", ")
        arg_definition_strs =
            map((name, value) -> "$name = $value", arg_name_strs, ($(args...),))
        arg_definitions_str = join(arg_definition_strs, '\n')
        unrolled_command_str = """
            using UnrolledUtilities
            unrolled_func($arg_names_str) = $($unrolled_expr_str)
            $arg_definitions_str
            stats1 = @timed unrolled_func($arg_names_str)
            stats2 = @timed unrolled_func($arg_names_str)
            print(stats1.time - stats2.time, ',', stats1.bytes - stats2.bytes)
            """
        reference_command_str = """
            reference_func($arg_names_str) = $($reference_expr_str)
            $arg_definitions_str
            stats1 = @timed reference_func($arg_names_str)
            stats2 = @timed reference_func($arg_names_str)
            print(stats1.time - stats2.time, ',', stats1.bytes - stats2.bytes)
            """

        # Get the unrolled function's time-to-first-run and its memory usage.
        run(pipeline(`julia --project -e $unrolled_command_str`, buffer))
        unrolled_time, unrolled_memory =
            parse.((Float64, Int), split(String(take!(buffer)), ','))

        # Make a new buffer to avoid a potential data race:
        # https://discourse.julialang.org/t/iobuffer-becomes-not-writable-after-run/92323/3
        close(buffer)
        buffer = IOBuffer()

        # Get the reference function's time-to-first-run and its memory usage.
        run(pipeline(`julia --project -e $reference_command_str`, buffer))
        reference_time, reference_memory =
            parse.((Float64, Int), split(String(take!(buffer)), ','))

        close(buffer)

        # Record all relevant information in comparison_table_dict.
        unrolled_performance_str = if !is_unrolled_const
            "type-stable"
        elseif !is_unrolled_optimized_out
            "const return value"
        else
            "fully optimized out"
        end
        reference_performance_str = if !is_reference_non_allocating
            "allocating"
        elseif !is_reference_stable
            "type-unstable"
        elseif !is_reference_const
            "type-stable"
        elseif !is_reference_optimized_out
            "const return value"
        else
            "fully optimized out"
        end
        time_ratio = unrolled_time / reference_time
        time_ratio_str = if time_ratio >= 1.5
            "$(round(Int, time_ratio)) times slower"
        elseif inv(time_ratio) >= 1.5
            "$(round(Int, inv(time_ratio))) times faster"
        else
            "similar"
        end
        memory_ratio = unrolled_memory / reference_memory
        memory_ratio_str = if memory_ratio >= 1.5
            "$(round(Int, memory_ratio)) times more"
        elseif inv(memory_ratio) >= 1.5
            "$(round(Int, inv(memory_ratio))) times less"
        else
            "similar"
        end
        dict_key = ($unrolled_expr_str, $reference_expr_str)
        dict_entry = (
            $(esc(contents_info_str)),
            unrolled_performance_str,
            reference_performance_str,
            time_ratio_str,
            memory_ratio_str,
        )
        if dict_key in keys(comparison_table_dict)
            push!(comparison_table_dict[dict_key], dict_entry)
        else
            comparison_table_dict[dict_key] = [dict_entry]
        end
    end
end

@testset "empty iterators" begin
    itr = ()
    str = "nothing"
    @test_unrolled (itr,) unrolled_any(error, itr) any(error, itr) str
    @test_unrolled (itr,) unrolled_all(error, itr) all(error, itr) str
    @test_unrolled (itr,) unrolled_foreach(error, itr) foreach(error, itr) str
    @test_unrolled (itr,) unrolled_map(error, itr, itr) map(error, itr, itr) str
    @test_unrolled(
        (itr,),
        unrolled_reduce(error, itr; init = 0),
        reduce(error, itr; init = 0),
        str,
    )
end

for n in (1, 8, 32, 33, 128), identical in (n == 1 ? (true,) : (true, false))
    itr1 = ntuple(i -> ntuple(Val, identical ? 0 : (i - 1) % 7), n)
    itr2 = ntuple(i -> ntuple(Val, identical ? 1 : (i - 1) % 7 + 1), n)
    itr3 = ntuple(i -> ntuple(identity, identical ? 1 : (i - 1) % 7 + 1), n)
    if n == 1
        str1 = "1 empty tuple"
        str2 = "1 nonempty singleton tuple"
        str3 = "1 nonempty non-singleton tuple"
        str12 = "1 singleton tuple"
        str23 = "1 nonempty tuple"
        str123 = "1 tuple"
    elseif identical
        str1 = "$n empty tuples"
        str2 = "$n identical nonempty singleton tuples"
        str3 = "$n identical nonempty non-singleton tuples"
        str12 = "$n identical singleton tuples"
        str23 = "$n identical nonempty tuples"
        str123 = "$n identical tuples"
    else
        str1 = "$n empty and nonempty singleton tuples"
        str2 = "$n nonempty singleton tuples"
        str3 = "$n nonempty non-singleton tuples"
        str12 = "$n singleton tuples"
        str23 = "$n nonempty tuples"
        str123 = "$n tuples"
    end
    @testset "iterators of $str123" begin
        for (itr, str) in ((itr1, str1), (itr2, str2), (itr3, str3))
            @test_unrolled (itr,) unrolled_any(isempty, itr) any(isempty, itr) str
            @test_unrolled (itr,) unrolled_any(!isempty, itr) any(!isempty, itr) str

            @test_unrolled (itr,) unrolled_all(isempty, itr) all(isempty, itr) str
            @test_unrolled (itr,) unrolled_all(!isempty, itr) all(!isempty, itr) str

            @test_unrolled(
                (itr,),
                unrolled_foreach(x -> @assert(length(x) <= 7), itr),
                foreach(x -> @assert(length(x) <= 7), itr),
                str,
            )

            @test_unrolled (itr,) unrolled_map(length, itr) map(length, itr) str

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

            @test_unrolled (itr,) unrolled_zip(itr) Tuple(zip(itr)) str

            @test_unrolled (itr,) unrolled_in(nothing, itr) (nothing in itr) str
            @test_unrolled (itr,) unrolled_in(itr[1], itr) (itr[1] in itr) str
            @test_unrolled (itr,) unrolled_in(itr[end], itr) (itr[end] in itr) str

            # unrolled_unique is only type-stable for singletons
            if Base.issingletontype(typeof(itr))
                @test_unrolled (itr,) unrolled_unique(itr) Tuple(unique(itr)) str
            end

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

            @test_unrolled(
                (itr,),
                unrolled_product(itr),
                Tuple(Iterators.product(itr)),
                str,
            )

            if n > 1
                @test_unrolled(
                    (itr,),
                    unrolled_take(itr, Val(7)),
                    itr[1:7],
                    str,
                )
                @test_unrolled(
                    (itr,),
                    unrolled_drop(itr, Val(7)),
                    itr[8:end],
                    str,
                )
            end
        end

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
                (x1, x2) -> @assert(length(x1) < length(x2)),
                itr1,
                itr2,
            ),
            foreach((x1, x2) -> @assert(length(x1) < length(x2)), itr1, itr2),
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
            unrolled_zip(itr1, itr2),
            Tuple(zip(itr1, itr2)),
            str12,
        )
        @test_unrolled(
            (itr1, itr2, itr3),
            unrolled_zip(itr1, itr2, itr3),
            Tuple(zip(itr1, itr2, itr3)),
            str123,
        )

        # unrolled_product can take several minutes to compile when n is large
        if n <= 33
            @test_unrolled(
                (itr1, itr2),
                unrolled_product(itr1, itr2),
                Tuple(Iterators.product(itr1, itr2)),
                str12,
            )
        end
        if n <= 8
            @test_unrolled(
                (itr1, itr2, itr3),
                unrolled_product(itr1, itr2, itr3),
                Tuple(Iterators.product(itr1, itr2, itr3)),
                str123,
            )
        end
    end
end

nested_iterator(depth, n, inner_n) =
    depth == 1 ? ntuple(identity, n) :
    ntuple(inner_n) do _
        nested_iterator(depth - 1, Int(n / inner_n), inner_n)
    end

for n in (8, 32, 128)
    @testset "iterators of $n values in nested tuples" begin
        for depth in (2, 3, 4:2:(Int(log2(n)) + 1)...)
            itr = nested_iterator(depth, n, 2)
            str = "$n values in nested tuples of depth $depth"
            # In the following definitions, use var"#self#" to avoid boxing:
            # https://discourse.julialang.org/t/performant-recursive-anonymous-functions/90984/5
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
