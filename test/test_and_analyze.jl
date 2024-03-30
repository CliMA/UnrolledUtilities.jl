using Test
using JET
using OrderedCollections
using PrettyTables

using UnrolledUtilities

measurements_dict = OrderedDict()

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
    unrolled_expr_str =
        replace(string(unrolled_expr), r"\s*#=.+=#" => "", r"\s+" => ' ')
    reference_expr_str =
        replace(string(reference_expr), r"\s*#=.+=#" => "", r"\s+" => ' ')
    expr_info_str =
        length(args) == 1 ? "$unrolled_expr_str with 1 iterator that contains" :
        "$unrolled_expr_str with $(length(args)) iterators that each contain"
    quote
        @info "Testing $($expr_info_str) $($(esc(contents_info_str)))"

        unrolled_func($(arg_names...)) = $unrolled_expr
        reference_func($(arg_names...)) = $reference_expr

        # Test for correctness.
        @test unrolled_func($(args...)) == reference_func($(args...))

        unrolled_func_and_nothing($(arg_names...)) = ($unrolled_expr; nothing)
        reference_func_and_nothing($(arg_names...)) = ($reference_expr; nothing)

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

        arg_name_strs = ($(map(string, arg_names)...),)
        arg_names_str = join(arg_name_strs, ", ")
        arg_definition_strs =
            map((name, value) -> "$name = $value", arg_name_strs, ($(args...),))
        arg_definitions_str = join(arg_definition_strs, '\n')
        unrolled_command_str = """
            using UnrolledUtilities
            unrolled_func($arg_names_str) = $($unrolled_expr_str)
            $arg_definitions_str
            stats = @timed unrolled_func($arg_names_str)
            print(stats.time, ',', stats.bytes)
            """
        reference_command_str = """
            reference_func($arg_names_str) = $($reference_expr_str)
            $arg_definitions_str
            stats = @timed reference_func($arg_names_str)
            print(stats.time, ',', stats.bytes)
            """

        # Get the compilation times and allocations.
        buffer1 = IOBuffer()
        run(pipeline(`julia --project -e $unrolled_command_str`, buffer1))
        unrolled_time, unrolled_allocs =
            parse.((Float64, Int), split(String(take!(buffer1)), ','))
        close(buffer1)
        buffer2 = IOBuffer()
        run(pipeline(`julia --project -e $reference_command_str`, buffer2))
        reference_time, reference_allocs =
            parse.((Float64, Int), split(String(take!(buffer2)), ','))
        close(buffer2)

        # Record all of the measurements.
        unrolled_performance_str =
            is_unrolled_const ? "constant" : "type-stable"
        reference_performance_str = if !is_reference_non_allocating
            "allocating"
        elseif !is_reference_stable
            "type-unstable"
        else
            is_reference_const ? "constant" : "type-stable"
        end
        time_ratio = unrolled_time / reference_time
        time_ratio_str = if time_ratio >= 1.5
            "$(round(Int, time_ratio)) times slower"
        elseif inv(time_ratio) >= 1.5
            "$(round(Int, inv(time_ratio))) times faster"
        else
            "similar"
        end
        allocs_ratio = unrolled_allocs / reference_allocs
        allocs_ratio_str = if allocs_ratio >= 1.5
            "$(round(Int, allocs_ratio)) times more"
        elseif inv(allocs_ratio) >= 1.5
            "$(round(Int, inv(allocs_ratio))) times less"
        else
            "similar"
        end
        measurement_key = ($unrolled_expr_str, $reference_expr_str)
        measurement_entry = (
            $(esc(contents_info_str)),
            unrolled_performance_str,
            reference_performance_str,
            time_ratio_str,
            allocs_ratio_str,
        )
        if measurement_key in keys(measurements_dict)
            push!(measurements_dict[measurement_key], measurement_entry)
        else
            measurements_dict[measurement_key] = [measurement_entry]
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

for n in (1, 10, 33), all_identical in (n == 1 ? (true,) : (true, false))
    itr1 = ntuple(i -> ntuple(Val, all_identical ? 0 : (i - 1) % 7), n)
    itr2 = ntuple(i -> ntuple(Val, all_identical ? 1 : (i - 1) % 7 + 1), n)
    itr3 = ntuple(i -> ntuple(identity, all_identical ? 1 : (i - 1) % 7 + 1), n)
    if n == 1
        str1 = "1 empty tuple"
        str2 = "1 nonempty singleton tuple"
        str3 = "1 nonempty non-singleton tuple"
        str12 = "1 singleton tuple"
        str23 = "1 nonempty tuple"
        str123 = "1 tuple"
    elseif all_identical
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
                unrolled_foreach(x -> (@assert length(x) <= 7), itr),
                foreach(x -> (@assert length(x) <= 7), itr),
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
                (x1, x2) -> (@assert length(x1) < length(x2)),
                itr1,
                itr2,
            ),
            foreach((x1, x2) -> (@assert length(x1) < length(x2)), itr1, itr2),
            str12,
        )
        @test_unrolled(
            (itr2, itr3),
            unrolled_foreach(
                (x2, x3) -> (@assert x2 == unrolled_map(Val, x3)),
                itr2,
                itr3,
            ),
            foreach((x2, x3) -> (@assert x2 == map(Val, x3)), itr2, itr3),
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

        @test_unrolled(
            (itr1, itr2),
            unrolled_product(itr1, itr2),
            Tuple(Iterators.product(itr1, itr2)),
            str12,
        )
        if n <= 10 # This can take several minutes to compile when n is large.
            @test_unrolled(
                (itr1, itr2, itr3),
                unrolled_product(itr1, itr2, itr3),
                Tuple(Iterators.product(itr1, itr2, itr3)),
                str123,
            )
        end
    end
end

table_data = mapreduce(vcat, collect(measurements_dict)) do (key, entries)
    stack(entry -> (key..., entry...), entries; dims = 1)
end
header_line1 = [
    "Unrolled Expression",
    "Reference Expression",
    "Iterator Contents",
    "Unrolled Performance",
    "Reference Performance",
    "Compilation Time",
    "Compilation Memory",
]
header_line2 =
    ["", "", "", "", "", "(Unrolled vs. Reference)", "(Unrolled vs. Reference)"]
better_performance_but_harder_to_compile =
    Highlighter(crayon"blue") do data, i, j
        data[i, 4] != data[i, 5] &&
            (endswith(data[i, 6], "slower") || endswith(data[i, 7], "more"))
    end
better_performance =
    Highlighter((data, i, j) -> data[i, 4] != data[i, 5], crayon"green")
harder_to_compile = Highlighter(crayon"red") do data, i, j
    endswith(data[i, 6], "slower") || endswith(data[i, 7], "more")
end
easier_to_compile = Highlighter(crayon"magenta") do data, i, j
    endswith(data[i, 6], "faster") || endswith(data[i, 7], "less")
end
no_difference = Highlighter((data, i, j) -> true, crayon"yellow")
pretty_table(
    table_data;
    title = "Comparison between UnrolledUtilities and Base/Base.Iterators",
    header = (header_line1, header_line2),
    subheader_crayon = crayon"bold",
    highlighters = (
        better_performance_but_harder_to_compile,
        better_performance,
        harder_to_compile,
        easier_to_compile,
        no_difference,
    ),
    title_same_width_as_table = true,
    title_alignment = :c,
    alignment = :l,
    columns_width = [45, 45, 0, 0, 0, 0, 0],
    crop = :none,
)
