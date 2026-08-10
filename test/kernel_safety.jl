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
# The iterators are `const` so that reading them is type-stable: allocations
# measured through a non-constant global are allocations of the global's boxed
# value, not of the function being tested.

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
discard(f) = (f(); nothing)

const COVERED = Set{Symbol}()

# The thunk is passed as an argument so that it is specialized on, which makes
# the analyses below independent of how this function is called.
function test_kernel_safety(name::Symbol, thunk)
    push!(COVERED, name)
    @testset "$name" begin
        @test_opt thunk()
        @test isconcretetype(Base.promote_op(thunk))
        discard(thunk) # run once to compile
        @test (@allocated discard(thunk)) == 0
    end
end

@testset "kernel safety on wide heterogeneous iterators" begin
    test_kernel_safety(:unrolled_map, () -> unrolled_map(identity, WIDE))
    test_kernel_safety(:unrolled_any, () -> unrolled_any(is_int, WIDE))
    test_kernel_safety(:unrolled_all, () -> unrolled_all(is_int, WIDE))
    test_kernel_safety(:unrolled_foreach, () -> unrolled_foreach(one_of, WIDE))
    test_kernel_safety(:unrolled_in, () -> unrolled_in(5, WIDE))
    test_kernel_safety(:unrolled_count, () -> unrolled_count(is_int, WIDE))

    # The init values are passed positionally, as they must be in GPU kernels.
    test_kernel_safety(
        :unrolled_reduce,
        () -> unrolled_reduce(count_up, WIDE, Init(0)),
    )
    test_kernel_safety(
        :unrolled_mapreduce,
        () -> unrolled_mapreduce(one_of, +, Init(0), WIDE),
    )
    test_kernel_safety(
        :unrolled_accumulate,
        () -> unrolled_accumulate(count_up, WIDE, 0),
    )
    test_kernel_safety(:unrolled_sum, () -> unrolled_sum(one_of, WIDE, 0))
    test_kernel_safety(:unrolled_prod, () -> unrolled_prod(one_of, WIDE, 1))
    test_kernel_safety(:unrolled_cumsum, () -> unrolled_cumsum(one_of, WIDE))
    test_kernel_safety(:unrolled_cumprod, () -> unrolled_cumprod(one_of, WIDE))

    test_kernel_safety(:unrolled_maximum, () -> unrolled_maximum(one_of, WIDE))
    test_kernel_safety(:unrolled_minimum, () -> unrolled_minimum(one_of, WIDE))
    test_kernel_safety(:unrolled_extrema, () -> unrolled_extrema(one_of, WIDE))
    test_kernel_safety(:unrolled_findmax, () -> unrolled_findmax(one_of, WIDE))
    test_kernel_safety(:unrolled_findmin, () -> unrolled_findmin(one_of, WIDE))
    test_kernel_safety(:unrolled_argmax, () -> unrolled_argmax(one_of, WIDE))
    test_kernel_safety(:unrolled_argmin, () -> unrolled_argmin(one_of, WIDE))

    test_kernel_safety(
        :unrolled_findfirst,
        () -> unrolled_findfirst(is_int, WIDE),
    )
    test_kernel_safety(
        :unrolled_findlast,
        () -> unrolled_findlast(is_int, WIDE),
    )
    test_kernel_safety(
        :unrolled_argfirst,
        () -> unrolled_argfirst(is_int, WIDE),
    )
    test_kernel_safety(:unrolled_arglast, () -> unrolled_arglast(is_int, WIDE))

    # unrolled_applyat is indexed at run time, which makes it the only unrolled
    # function that compiles to a switch statement rather than straight-line
    # code, so it is the most sensitive to the width of its iterator.
    test_kernel_safety(
        :unrolled_applyat,
        () -> unrolled_applyat(one_of, 7, WIDE),
    )

    test_kernel_safety(:unrolled_filter, () -> unrolled_filter(is_int, WIDE))
    test_kernel_safety(:unrolled_split, () -> unrolled_split(is_int, WIDE))
    test_kernel_safety(:unrolled_unique, () -> unrolled_unique(WIDE))
    test_kernel_safety(:unrolled_allunique, () -> unrolled_allunique(WIDE))
    test_kernel_safety(:unrolled_allequal, () -> unrolled_allequal(WIDE))

    test_kernel_safety(:unrolled_flatten, () -> unrolled_flatten(NESTED))
    test_kernel_safety(
        :unrolled_flatmap,
        () -> unrolled_flatmap(x -> (x, x), WIDE),
    )
    test_kernel_safety(:unrolled_cycle, () -> unrolled_cycle(WIDE, Val(2)))
    test_kernel_safety(
        :unrolled_partition,
        () -> unrolled_partition(WIDE, Val(8)),
    )

    test_kernel_safety(:unrolled_push, () -> unrolled_push(WIDE, 1))
    test_kernel_safety(:unrolled_append, () -> unrolled_append(WIDE, WIDE))
    test_kernel_safety(:unrolled_prepend, () -> unrolled_prepend(WIDE, WIDE))
    test_kernel_safety(:unrolled_take, () -> unrolled_take(WIDE, Val(16)))
    test_kernel_safety(:unrolled_drop, () -> unrolled_drop(WIDE, Val(16)))

    # A product of the full iterator would have 33^2 items, so this uses a
    # short iterator whose item types are still heterogeneous.
    test_kernel_safety(
        :unrolled_product,
        () -> unrolled_product(SHORT_HETEROGENEOUS, SHORT_HETEROGENEOUS),
    )
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
