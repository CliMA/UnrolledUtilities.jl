using Test
using JET
using UnrolledUtilities

# The kernel-safety contract: when an unrolled function is called on a wide
# iterator with heterogeneous item types, it must be
#   1. type-stable (no runtime dispatch),
#   2. free of allocations, and
#   3. inferred to return a concrete type,
# since dynamic dispatch and heap allocation cannot be compiled for GPUs. The
# fourth guarantee, that results constant-fold for constant inputs, is asserted
# in container_and_callable.jl for the functions whose results are used as type
# parameters.
#
# The iterators below are deliberately wide (33 items) and heterogeneous (13
# distinct item types). Inference's recursion-widening heuristics and
# union-splitting limits are only exceeded by iterators of this shape, so
# narrower or homogeneous iterators cannot detect the regressions these tests
# guard against.
#
# Each function is called with its iterator as an argument, as a kernel would
# call it, so that only the iterator's type is available to inference. Closing
# over a `const` iterator instead would let the values themselves constant
# fold, which hides every dependence on run-time values: a kernel receives its
# iterators as arguments, and no folding is available there.

const WIDE = (
    ntuple(i -> Val(i), 11)...,
    ntuple(i -> i, 11)...,
    ntuple(i -> Float32(i), 11)...,
)
const NESTED = ntuple(i -> ntuple(j -> Val(j), i), 8)
const SHORT_HETEROGENEOUS = (1, 1.0f0, Val(1))

is_int(x) = x isa Int
one_of(x) = 1
count_up(accumulated, _) = accumulated + 1

# Allocations are measured with the result discarded, since returning an
# iterator that is too large for the argument registers allocates a boxed copy
# of it regardless of how the function itself is implemented.
discard(f::F, itr) where {F} = (f(itr); nothing)

const COVERED = Set{Symbol}()

# The function is passed as an argument so that it is specialized on, which
# makes the analyses below independent of how this function is called.
function test_kernel_safety(name::Symbol, f::F, itr = WIDE) where {F}
    push!(COVERED, name)
    @testset "$name" begin
        @test_opt f(itr)
        @test isconcretetype(Base.promote_op(f, typeof(itr)))
        discard(f, itr) # run once to compile
        @test (@allocated discard(f, itr)) == 0
    end
end

@testset "kernel safety on wide heterogeneous iterators" begin
    test_kernel_safety(:unrolled_map, itr -> unrolled_map(identity, itr))
    test_kernel_safety(:unrolled_any, itr -> unrolled_any(is_int, itr))
    test_kernel_safety(:unrolled_all, itr -> unrolled_all(is_int, itr))
    test_kernel_safety(:unrolled_foreach, itr -> unrolled_foreach(one_of, itr))
    test_kernel_safety(:unrolled_in, itr -> unrolled_in(5, itr))
    test_kernel_safety(:unrolled_count, itr -> unrolled_count(is_int, itr))

    # The init values are passed positionally, as they must be in GPU kernels.
    test_kernel_safety(
        :unrolled_reduce,
        itr -> unrolled_reduce(count_up, itr, Init(0)),
    )
    test_kernel_safety(
        :unrolled_mapreduce,
        itr -> unrolled_mapreduce(one_of, +, Init(0), itr),
    )
    test_kernel_safety(
        :unrolled_accumulate,
        itr -> unrolled_accumulate(count_up, itr, 0),
    )
    test_kernel_safety(:unrolled_sum, itr -> unrolled_sum(one_of, itr, 0))
    test_kernel_safety(:unrolled_prod, itr -> unrolled_prod(one_of, itr, 1))
    test_kernel_safety(:unrolled_cumsum, itr -> unrolled_cumsum(one_of, itr))
    test_kernel_safety(:unrolled_cumprod, itr -> unrolled_cumprod(one_of, itr))

    test_kernel_safety(:unrolled_maximum, itr -> unrolled_maximum(one_of, itr))
    test_kernel_safety(:unrolled_minimum, itr -> unrolled_minimum(one_of, itr))
    test_kernel_safety(:unrolled_extrema, itr -> unrolled_extrema(one_of, itr))
    test_kernel_safety(:unrolled_findmax, itr -> unrolled_findmax(one_of, itr))
    test_kernel_safety(:unrolled_findmin, itr -> unrolled_findmin(one_of, itr))
    test_kernel_safety(:unrolled_argmax, itr -> unrolled_argmax(one_of, itr))
    test_kernel_safety(:unrolled_argmin, itr -> unrolled_argmin(one_of, itr))

    test_kernel_safety(
        :unrolled_findfirst,
        itr -> unrolled_findfirst(is_int, itr),
    )
    test_kernel_safety(
        :unrolled_findlast,
        itr -> unrolled_findlast(is_int, itr),
    )
    test_kernel_safety(
        :unrolled_argfirst,
        itr -> unrolled_argfirst(is_int, itr),
    )
    test_kernel_safety(:unrolled_arglast, itr -> unrolled_arglast(is_int, itr))

    # unrolled_applyat is indexed at run time, which makes it the only unrolled
    # function that compiles to a switch statement rather than straight-line
    # code, so it is the most sensitive to the width of its iterator.
    test_kernel_safety(
        :unrolled_applyat,
        itr -> unrolled_applyat(one_of, 7, itr),
    )

    test_kernel_safety(:unrolled_filter, itr -> unrolled_filter(is_int, itr))
    test_kernel_safety(:unrolled_split, itr -> unrolled_split(is_int, itr))
    # Uniqueness is determined by type rather than by value here; see the
    # testset that follows this one.
    test_kernel_safety(:unrolled_unique, itr -> unrolled_unique(typeof, itr))
    test_kernel_safety(
        :unrolled_allunique,
        itr -> unrolled_allunique(typeof, itr),
    )
    test_kernel_safety(:unrolled_allequal, itr -> unrolled_allequal(itr))

    test_kernel_safety(:unrolled_flatten, unrolled_flatten, NESTED)
    test_kernel_safety(
        :unrolled_flatmap,
        itr -> unrolled_flatmap(x -> (x, x), itr),
    )
    test_kernel_safety(:unrolled_cycle, itr -> unrolled_cycle(itr, Val(2)))
    test_kernel_safety(
        :unrolled_partition,
        itr -> unrolled_partition(itr, Val(8)),
    )

    test_kernel_safety(:unrolled_push, itr -> unrolled_push(itr, 1))
    test_kernel_safety(:unrolled_append, itr -> unrolled_append(itr, WIDE))
    test_kernel_safety(:unrolled_prepend, itr -> unrolled_prepend(itr, WIDE))
    test_kernel_safety(:unrolled_take, itr -> unrolled_take(itr, Val(16)))
    test_kernel_safety(:unrolled_drop, itr -> unrolled_drop(itr, Val(16)))

    # A product of the full iterator would have 33^2 items, so this uses a
    # short iterator whose item types are still heterogeneous.
    test_kernel_safety(
        :unrolled_product,
        itr -> unrolled_product(itr, itr),
        SHORT_HETEROGENEOUS,
    )
end

# When uniqueness is determined by value, the number of unique items depends on
# the values of the items, so the type of the result depends on them as well.
# This is a property of the operation rather than of its implementation, so
# unrolled_unique and unrolled_allunique can only be called from kernels when
# uniqueness is determined by type, or by some other function of the item types.
# The assertions below fail if this ever stops being the case, which would mean
# that the corpus above can use the value-based forms.
@testset "value-based uniqueness is not kernel-safe" begin
    value_unique(itr) = unrolled_unique(itr)
    value_allunique(itr) = unrolled_allunique(itr)
    @test !isconcretetype(Base.promote_op(value_unique, typeof(WIDE)))
    @test !isempty(JET.get_reports(@report_opt value_unique(WIDE)))
    @test !isempty(JET.get_reports(@report_opt value_allunique(WIDE)))

    # Uniqueness by type is kernel-safe for the same iterator.
    type_unique(itr) = unrolled_unique(typeof, itr)
    @test isconcretetype(Base.promote_op(type_unique, typeof(WIDE)))
    @test isempty(JET.get_reports(@report_opt type_unique(WIDE)))
end

# A function that is exported but not covered above is a function whose
# kernel safety is unverified.
@testset "every exported unrolled function is covered" begin
    exported_functions = Set(
        name for name in names(UnrolledUtilities) if
        startswith(string(name), "unrolled_")
    )
    uncovered = sort(collect(setdiff(exported_functions, COVERED)))
    isempty(uncovered) || @show uncovered
    @test isempty(uncovered)
end

# Keyword arguments are lowered into calls to Core.kwcall, which does not
# always specialize during GPU compilation of complexly typed arguments, and an
# unspecialized kwcall is a dynamic invocation that cannot be compiled. Keyword
# arguments are therefore only allowed in the outermost user-facing methods,
# which pass their values on positionally, so no method in this package should
# contain a call to Core.kwcall.
@testset "no keyword-argument calls between unrolled functions" begin
    has_kwcall(instructions) =
        any(instructions) do instruction
            instruction isa Expr && any(instruction.args) do argument
                argument isa GlobalRef && argument.name === :kwcall
            end
        end
    offending_methods = String[]
    for name in names(UnrolledUtilities; all = true)
        isdefined(UnrolledUtilities, name) || continue
        function_or_value = getproperty(UnrolledUtilities, name)
        function_or_value isa Function || continue
        for method in methods(function_or_value)
            instructions = try
                Base.uncompressed_ir(method).code
            catch
                continue
            end
            has_kwcall(instructions) && push!(
                offending_methods,
                "$name at $(method.file):$(method.line)",
            )
        end
    end
    isempty(offending_methods) || @show offending_methods
    @test isempty(offending_methods)
end
