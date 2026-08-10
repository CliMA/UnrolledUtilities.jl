using Test
using CUDA
using UnrolledUtilities

# GPU compilation is the only place where the kernel-safety contract is fully
# enforced: inference's recursion-widening heuristics, its union-splitting
# limits, and the specialization of keyword-argument calls all have thresholds
# that CPU analyses like JET do not reach, so code that JET reports as clean can
# still fail to compile for a GPU. Each test below compiles a kernel whose body
# exercises one of the patterns that has broken GPU compilation of downstream
# code.
#
# Compiling a kernel is the assertion: `@cuda launch = false` throws an
# InvalidIRError if the kernel's IR contains a dynamic dispatch or a heap
# allocation. Only the last test launches a kernel, to check that the results
# computed on a device match the results computed on a host.

if !CUDA.functional()
    @warn "CUDA is not functional; skipping GPU kernel safety tests"
    exit(0)
end

const WIDE = (
    ntuple(i -> Val(i), 11)...,
    ntuple(i -> i, 11)...,
    ntuple(i -> Float32(i), 11)...,
)
const SHORT_HETEROGENEOUS = (1, 1.0f0, Val(1))

is_int(x) = x isa Int
one_of(x) = 1
count_up(accumulated, _) = accumulated + 1

compiles(kernel, args...) = (CUDA.@cuda launch = false kernel(args...); true)

# Selecting items from a wide iterator with heterogeneous item types is the
# pattern that broke ClimaAtmos: implementations that push items into an
# accumulator whose type grows on every step make this kernel fail to compile.
function selection_kernel!(out, itr)
    @inbounds out[1] =
        unrolled_sum(one_of, unrolled_filter(is_int, itr), 0) +
        unrolled_sum(one_of, unrolled_unique(itr), 0) +
        unrolled_sum(one_of, first(unrolled_split(is_int, itr)), 0)
    return nothing
end

# Initial values must reach a reduction positionally: a keyword argument is
# lowered into a call to Core.kwcall, which does not always specialize here.
function positional_init_kernel!(out, itr)
    @inbounds out[1] =
        unrolled_reduce(count_up, itr, Init(0)) +
        unrolled_mapreduce(one_of, +, Init(0), itr) +
        unrolled_sum(one_of, itr, 0)
    return nothing
end

# Products of iterators with heterogeneous item types index those iterators at
# positions that are only known after constant propagation.
function product_kernel!(out, itr1, itr2)
    @inbounds out[1] = unrolled_sum(one_of, unrolled_product(itr1, itr2), 0)
    return nothing
end

# unrolled_applyat is indexed at run time, so it compiles to a switch statement
# whose size grows with the width of the iterator.
function applyat_kernel!(out, itr)
    index = (CUDA.threadIdx().x - 1) % length(itr) + 1
    @inbounds out[1] = unrolled_applyat(one_of, index, itr)
    return nothing
end

# StaticBitVectors pack their items into Unsigned chunks, which requires the
# chunk indices to constant-fold.
function bitvector_kernel!(out, bitvector)
    mapped = unrolled_map(!, bitvector)
    @inbounds out[1] = unrolled_count(identity, mapped)
    return nothing
end

# Errors thrown from kernels must not build their messages inside the kernel:
# only a singleton exception type is known to compile, since interpolating
# values into a string allocates.
struct SingletonKernelError{value} <: Exception end
Base.showerror(io::IO, ::SingletonKernelError{value}) where {value} =
    print(io, "kernel error: $value")

function singleton_throw_kernel!(out, itr)
    unrolled_all(is_int, itr) && throw(SingletonKernelError{:not_all_ints}())
    @inbounds out[1] = 1
    return nothing
end

@testset "GPU kernel safety" begin
    out = CUDA.zeros(Int, 1)
    bitvector = StaticBitVector{256}(isodd)

    @testset "item selection" begin
        @test compiles(selection_kernel!, out, WIDE)
    end
    @testset "positional init values" begin
        @test compiles(positional_init_kernel!, out, WIDE)
    end
    @testset "heterogeneous product" begin
        @test compiles(
            product_kernel!,
            out,
            SHORT_HETEROGENEOUS,
            SHORT_HETEROGENEOUS,
        )
    end
    @testset "run-time index" begin
        @test compiles(applyat_kernel!, out, WIDE)
    end
    @testset "StaticBitVector" begin
        @test compiles(bitvector_kernel!, out, bitvector)
    end
    @testset "singleton exception type" begin
        @test compiles(singleton_throw_kernel!, out, WIDE)
    end

    # Whether a kernel can throw an error whose message is built inside the
    # kernel is not asserted, because it is not yet known which of these
    # patterns GPUCompiler accepts. The results are logged so that the first
    # run of this job answers the question, after which these can become
    # assertions and, if a constant string compiles, the singleton exception
    # types in downstream packages can be replaced by one parameterized type.
    @generated generated_message(::Val{name}) where {name} =
        "kernel error: $name"
    function interpolated_throw_kernel!(out, itr)
        unrolled_all(is_int, itr) && error("kernel error: $(length(itr)) items")
        @inbounds out[1] = 1
        return nothing
    end
    function generated_throw_kernel!(out, itr)
        unrolled_all(is_int, itr) &&
            error(generated_message(Val(:not_all_ints)))
        @inbounds out[1] = 1
        return nothing
    end
    for (description, kernel) in (
        ("an interpolated message", interpolated_throw_kernel!),
        ("a generated constant message", generated_throw_kernel!),
    )
        compiled = try
            compiles(kernel, out, WIDE)
        catch exception
            false
        end
        @info "A kernel that throws an error with $description \
               $(compiled ? "compiles" : "does not compile")"
    end

    @testset "results match the host" begin
        CUDA.@cuda threads = 1 selection_kernel!(out, WIDE)
        CUDA.synchronize()
        host_out = zeros(Int, 1)
        selection_kernel!(host_out, WIDE)
        @test Array(out) == host_out
    end
end
