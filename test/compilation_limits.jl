import CUDA, GPUCompiler
using Test
using UnrolledUtilities

@info """
Testing compilation of UnrolledUtilities v$(pkgversion(UnrolledUtilities)) with
  -   julia v$VERSION
  -   LLVM v$(Base.libllvm_version)
  -   CUDA.jl v$(pkgversion(CUDA))
  -   GPUCompiler.jl v$(pkgversion(GPUCompiler))"""

# A failing assertion that something compiles indicates a regression in either
# UnrolledUtilities, GPUCompiler, or the default Julia/LLVM compiler. A failing
# assertion that something does not compile indicates that an old compilation
# limit was lifted. A broken test indicates a temporarily uncategorized failure,
# corresponding to either a flaw in the package or some new compilation limit.

##################
## Test Harness ##
##################

# Hardcode compute capability of A100 GPU instead of looking for a device. Tests
# below should give identical results on 6.0/6.3, 7.0/7.8, 7.5/7.5, and 8.0/8.0.
const gpu_target = GPUCompiler.PTXCompilerTarget(; cap = v"8.0", ptx = v"8.0")

# CUDACore module exists since CUDA.jl v6. Use CUDA directly for v5 and earlier.
const gpu_params = if isdefined(CUDA, :CUDACore)
    sm = CUDA.CUDACore.SMVersion(gpu_target.cap.major, gpu_target.cap.minor)
    CUDA.CUDACore.CUDACompilerParams(; sm, gpu_target.ptx)
else
    CUDA.CUDACompilerParams(; gpu_target.cap, gpu_target.ptx)
end

# Setting libraries = false in the CompilerConfig below leaves the GPU's runtime
# and math libraries unlinked, so GPUCompiler reports calls into them as errors.
# Two types of calls can occur in valid kernels: exception handling and
# libdevice functions (__nv). Other errors reported by GPUCompiler are genuine
# compilation failures (e.g., gpu_gc_pool_alloc, which allocates heap memory).
is_allowed_ir_error((kind, _, meta)) =
    kind == GPUCompiler.UNKNOWN_FUNCTION &&
    (endswith(meta, "exception") || startswith(meta, "__nv"))

# A KernelError means one of the tests below asked for something impossible, so
# it is reported verbatim. An InvalidIRError indicates a compiler error, but
# only when is_allowed_ir_error returns false. Anything else is a harness error.
gpu_error_strings(err) = rethrow()
gpu_error_strings(err::GPUCompiler.KernelError) = [sprint(showerror, err)]
function gpu_error_strings(err::GPUCompiler.InvalidIRError)
    ir_errs = Iterators.filter(!is_allowed_ir_error, err.errors)
    return unique(Iterators.map(((kind, _, meta),) -> "$kind [$meta]", ir_errs))
end

# Gather all errors thrown by GPUCompiler for f(::arg_type1, ::arg_type2, ...).
# An empty result corresponds to successful IR generation and validation, which
# are the GPUCompiler stages that reject dynamic dispatch and heap allocations.
# The always_inline flag from CUDA.@cuda is also exposed; its default value is
# false, but ClimaCore sets it to true, so most results below must hold either
# way. See Compilation Limit 1 for an example that is affected by always_inline.
function gpu_errors(f, arg_types...; always_inline = false)
    try
        source = GPUCompiler.methodinstance(typeof(f), Tuple{arg_types...})
        kwargs = (; kernel = true, libraries = false, always_inline)
        config = GPUCompiler.CompilerConfig(gpu_target, gpu_params; kwargs...)
        compiler_job = GPUCompiler.CompilerJob(source, config)
        GPUCompiler.JuliaContext(_ -> GPUCompiler.compile(:llvm, compiler_job))
        return []
    catch err
        return gpu_error_strings(err)
    end
end

# GPU kernels must return nothing, and they must also store their results
# somewhere to avoid being elided out during code generation. So, every unrolled
# function passed to a kernel is wrapped in a closure that updates an array and
# returns nothing. Also, @allocated boxes results when called from a scope that
# cannot infer their types, so functions are similarly wrapped in allocation
# tests. Use a separate wrapper for unrolled_foreach, which has no return value.
storing(f) = (out, args...) -> (@inline; @inbounds out[1] = f(args...); nothing)
storing(::typeof(unrolled_foreach)) =
    (out, f, arg) -> unrolled_foreach(item -> (@inbounds out[1] = f(item)), arg)

return_type(f, args...) = Core.Compiler.return_type(f, typeof(args))
store_type(f, args...) = return_type(f, args...)
store_type(::typeof(unrolled_foreach), f, arg) = return_type(f, arg[1])

# Inline and force specialization on all arguments to avoid runtime dispatch.
@inline function store_allocs(f, args...; T = store_type(f, args...))
    out = Array{T, 1}(undef, 1) # allocate output array
    storing(f)(out, args...) # run once to compile
    return @allocated storing(f)(out, args...)
end
test_zero_allocs(f::F, x::X) where {F, X} = @test store_allocs(f, x) == 0
test_zero_allocs(f::F, x::X, y::Y) where {F, X, Y} =
    @test store_allocs(f, x, y) == 0
test_zero_allocs(f::F, x::X, y::Y, z::Z) where {F, X, Y, Z} =
    @test store_allocs(f, x, y, z) == 0

# Only the array type is needed for GPUCompiler, so no GPU array is constructed.
# Compare against [] instead of calling isempty so @test prints out every error.
function store_errors(f, args...; T = store_type(f, args...), kwargs...)
    out_type = CUDA.CuDeviceArray{T, 1, CUDA.AS.Global}
    return gpu_errors(storing(f), out_type, map(typeof, args)...; kwargs...)
end
test_gpu_compiles(f, args...; broken = false, kwargs...) =
    @test store_errors(f, args...; kwargs...) == [] broken = broken
test_gpu_throws(error_substring, f, args...; kwargs...) =
    @test any(contains(error_substring), store_errors(f, args...; kwargs...))
test_gpu_throws_alloc(f, args...; kwargs...) =
    test_gpu_throws("gpu_gc_pool_alloc", f, args...; kwargs...)

ints(n) = ntuple(identity, n)
floats(n) = ntuple(Float32, n)
vals(n) = ntuple(Val, n)

runtime_number(x) = x isa Val ? typeof(x).parameters[1] : x
runtime_bool(x) = isodd(Int(runtime_number(x)))
runtime_int(x) = Int(runtime_number(x))
runtime_tuple(x) = (runtime_number(x), runtime_bool(x), runtime_int(x))
runtime_combination(x, y) = runtime_number(x) + runtime_number(y)

# Functions that return values from heterogenous inputs must be inferrable, so
# they need to evaluate constant_number/bool instead of runtime_number/bool. See
# Compilation Limit 2 for examples of instabilities due to runtime predicates.
constant_number(x) = x isa Val ? typeof(x).parameters[1] : one(x)
constant_bool(x) = isodd(Int(constant_number(x)))

#####################
## Baseline Checks ##
#####################

# Use @noinline to ensure that a Ref is heap-allocated.
@noinline boxed_ref(n) = Ref(n)

# Use @generated to make string/symbol construction statically inferable.
@generated stable_error(x) = :(x > 0 ? x : error($("invalid input of type $x")))
unstable_error(x) = x > 0 ? x : error("invalid input of type $(typeof(x))")
@generated stable_val(x) = :(Val($(QuoteNode(Symbol(:input_of_type_, x)))))
unstable_val(x) = Val(Symbol(:input_of_type_, typeof(x)))

@testset "baseline: test harness verification" begin
    test_zero_allocs(n -> Ref(n)[], 1)
    test_gpu_compiles(n -> Ref(n)[], 1)
    test_gpu_throws_alloc(n -> boxed_ref(n)[], 1)

    test_zero_allocs(stable_error, 1)
    test_gpu_compiles(stable_error, 1)
    test_gpu_throws_alloc(unstable_error, 1)
    test_gpu_throws("ijl_alloc_string", unstable_error, 1)

    test_zero_allocs(stable_val, 1)
    test_gpu_compiles(stable_val, 1)
    test_gpu_throws_alloc(unstable_val, 1)
    test_gpu_throws("ijl_symbol_n", unstable_val, 1)
end

# Regardless of width and heterogeneity, every exported function must have an
# inferable return type, allocate nothing, and support compilation on GPUs.
# Failing tests that have not yet been categorized are flagged with gpu_broken.
test_unrolled(f, args...; gpu_broken = false) = @testset "$f" begin
    @test return_type(f, args...) === typeof(f(args...))
    test_zero_allocs(f, args...)
    test_gpu_compiles(f, args...; broken = gpu_broken)
end

@testset "baseline: wide and heterogenous inputs" begin
    WIDE = (floats(20)..., vals(20)..., ints(20)...) # 60 items of 22 types
    test_unrolled(unrolled_push, WIDE, 1)
    test_unrolled(unrolled_append, WIDE, WIDE)
    test_unrolled(unrolled_prepend, WIDE, WIDE)
    test_unrolled(unrolled_take, WIDE, Val(30))
    test_unrolled(unrolled_drop, WIDE, Val(30))
    test_unrolled(unrolled_setindex, WIDE, 1, Val(30))
    test_unrolled(unrolled_insert, WIDE, 1, Val(30))
    test_unrolled(unrolled_map, runtime_number, WIDE)
    test_unrolled(unrolled_any, runtime_bool, WIDE; gpu_broken = true)
    test_unrolled(unrolled_all, runtime_bool, WIDE; gpu_broken = true)
    test_unrolled(unrolled_foreach, runtime_number, WIDE)
    test_unrolled(unrolled_reduce, runtime_combination, WIDE)
    test_unrolled(unrolled_mapreduce, runtime_number, runtime_combination, WIDE)
    test_unrolled(unrolled_accumulate, runtime_combination, WIDE)
    test_unrolled(unrolled_applyat, runtime_int, 30, WIDE; gpu_broken = true)
    test_unrolled(unrolled_in, 1, WIDE)
    test_unrolled(unrolled_unique, constant_number, WIDE)
    test_unrolled(unrolled_allunique, runtime_number, WIDE)
    test_unrolled(unrolled_allequal, runtime_number, WIDE)
    test_unrolled(unrolled_sum, runtime_number, WIDE)
    test_unrolled(unrolled_prod, runtime_number, WIDE)
    test_unrolled(unrolled_cumsum, runtime_number, WIDE)
    test_unrolled(unrolled_cumprod, runtime_number, WIDE)
    test_unrolled(unrolled_count, runtime_bool, WIDE; gpu_broken = true)
    test_unrolled(unrolled_maximum, runtime_number, WIDE)
    test_unrolled(unrolled_minimum, runtime_number, WIDE)
    test_unrolled(unrolled_extrema, runtime_number, WIDE)
    test_unrolled(unrolled_findmax, runtime_int, WIDE; gpu_broken = true)
    test_unrolled(unrolled_findmin, runtime_int, WIDE; gpu_broken = true)
    test_unrolled(unrolled_argmax, constant_number, WIDE)
    test_unrolled(unrolled_argmin, constant_number, WIDE)
    test_unrolled(unrolled_findfirst, constant_bool, WIDE)
    test_unrolled(unrolled_findlast, constant_bool, WIDE)
    test_unrolled(unrolled_argfirst, constant_bool, WIDE)
    test_unrolled(unrolled_arglast, constant_bool, WIDE)
    test_unrolled(unrolled_filter, constant_bool, WIDE)
    test_unrolled(unrolled_split, constant_bool, WIDE)
    test_unrolled(unrolled_flatten, (WIDE, WIDE, WIDE))
    test_unrolled(unrolled_flatmap, runtime_tuple, WIDE; gpu_broken = true)
    test_unrolled(unrolled_product, WIDE, vals(3))
    test_unrolled(unrolled_cycle, WIDE, Val(3))
    test_unrolled(unrolled_partition, WIDE, Val(3))
end

@testset "baseline: wide StaticBitVector inputs" begin
    WIDE_BITS = StaticBitVector{256}(isodd)
    test_unrolled(unrolled_push, WIDE_BITS, true)
    test_unrolled(unrolled_append, WIDE_BITS, WIDE_BITS)
    test_unrolled(unrolled_prepend, WIDE_BITS, WIDE_BITS)
    test_unrolled(unrolled_take, WIDE_BITS, Val(30))
    test_unrolled(unrolled_drop, WIDE_BITS, Val(30))
    test_unrolled(unrolled_setindex, WIDE_BITS, true, Val(30))
    test_unrolled(unrolled_insert, WIDE_BITS, true, Val(30))
    test_unrolled(unrolled_map, !, WIDE_BITS)
    test_unrolled(unrolled_any, !, WIDE_BITS)
    test_unrolled(unrolled_all, !, WIDE_BITS)
    test_unrolled(unrolled_foreach, !, WIDE_BITS)
    test_unrolled(unrolled_reduce, &, WIDE_BITS)
    test_unrolled(unrolled_mapreduce, !, &, WIDE_BITS)
    test_unrolled(unrolled_accumulate, &, WIDE_BITS)
    test_unrolled(unrolled_applyat, !, 30, WIDE_BITS)
    # Higher-level unrolled functions are tested above with heterogenous inputs,
    # so there is no need to test them here with simpler StaticBitVector inputs.
end

combining(f) = (T, x) -> promote_type(T, f(x))

nested_reduce(::Val{T}) where {T} = T
nested_reduce(x) = unrolled_reduce(combining(nested_reduce), x; init = Union{})
nested_mapreduce(::Val{T}) where {T} = T
nested_mapreduce(x) =
    unrolled_mapreduce(nested_mapreduce, promote_type, x; init = Union{})
nested_accumulate(::Val{T}) where {T} = T
nested_accumulate(x) =
    isempty(x) ? Union{} :
    last(unrolled_accumulate(combining(nested_accumulate), x; init = Union{}))

# Every function that supports keyword arguments must drop the recursion limit
# from its Core.kwcall methods to avoid instabilities with nested iterators.
test_unrolled_recursion(f) = @testset "$f" begin
    NESTED = ((Val(Int), (Val(Float32),)), ((Val(Float64), Val(Int), ()),))
    @test return_type(f, NESTED) === Type{Float64}
    test_zero_allocs(one ∘ f, NESTED)
    test_gpu_compiles(one ∘ f, NESTED)
end

@testset "baseline: recursion with keyword arguments" begin
    test_unrolled_recursion(nested_reduce)
    test_unrolled_recursion(nested_mapreduce)
    test_unrolled_recursion(nested_accumulate)
end

#########################
## Compilation Limit 1 ##
#########################

# Every unrolled loop length must be determined from available iterators, a Val,
# or a propagated constant. Constants are only propagated through functions the
# compiler expects to inline, so inlining may be required to avoid allocations.
constant_sum(::Val{n}) where {n} = unrolled_sum(1:n)
runtime_sum(n) = unrolled_sum(1:n)
constant_sum(::Val{n}, x) where {n} = unrolled_sum(Base.Fix1(*, x), 1:n)
runtime_sum(n, x) = unrolled_sum(Base.Fix1(*, x), 1:n)
@inline inlined_sum(n, x) = unrolled_sum(Base.Fix1(*, x), 1:n)
@noinline noinlined_sum(n, x) = unrolled_sum(Base.Fix1(*, x), 1:n)

@testset "limit: constant propagation" begin
    # Inlining happens after type inference, so the return type can be ambiguous
    # even when the loop length is successfully propagated.
    T = Int

    # A literal is enough if the function is inlined; a Val is needed otherwise.
    for always_inline in (true, false)
        test_gpu_compiles(constant_sum, Val(9); always_inline)
        test_gpu_throws_alloc(runtime_sum, 9; T, always_inline)
        test_gpu_throws("box", runtime_sum, 9; T, always_inline)

        test_gpu_compiles(x -> constant_sum(Val(9), x), 3; always_inline)
        test_gpu_compiles(x -> inlined_sum(9, x), 3; always_inline)
        test_gpu_throws_alloc(x -> noinlined_sum(9, x), 3; T, always_inline)
        test_gpu_throws("box", x -> noinlined_sum(9, x), 3; T, always_inline)
    end

    # This is the only example affected by the always_inline flag from CUDA.jl.
    # The flag raises inline_cost_threshold to MAX_INLINE_COST, so the compiler
    # expects to inline unrolled_sum and can propagate the literal value of n.
    test_gpu_compiles(x -> runtime_sum(9, x), 3; T, always_inline = true)
    test_gpu_throws_alloc(x -> runtime_sum(9, x), 3; always_inline = true)
    test_gpu_throws_alloc(x -> runtime_sum(9, x), 3; T, always_inline = false)
    test_gpu_throws("box", x -> runtime_sum(9, x), 3; T, always_inline = false)
end

#########################
## Compilation Limit 2 ##
#########################

# In the examples above, the predicate is one the compiler folds item by item,
# because each item's type is known. A predicate that reads a runtime argument
# cannot be folded; types determined by such a predicate are inferred as Unions
# up to three possible options, but runtime dispatch occurs when there are more.
random_item_isbits(itr) = isbits(unrolled_argfirst(_ -> rand(Bool), itr))
runtime_num_unique(itr) = length(unrolled_unique(runtime_bool, itr))
runtime_num_filtered(itr) = length(unrolled_filter(runtime_bool, itr))

@testset "limit: runtime predicates" begin
    # Unions are preserved by unrolled_argfirst up to three different types.
    test_gpu_compiles(random_item_isbits, vals(3))
    test_gpu_throws("getfield", random_item_isbits, vals(4))

    # With four or more types, Julia 1.11 can generate stable code while 1.10
    # generates unstable code, but only if there is one type per typename.
    # Multiple types per typename lead to instabilities on both 1.10 and 1.11,
    # though the IR errors reported by GPUCompiler may differ across versions.
    WIDE_1_VAL = (floats(20)..., Val(1), ints(20)...)
    test_gpu_compiles(random_item_isbits, WIDE_1_VAL)
    if VERSION >= v"1.11"
        test_gpu_compiles(random_item_isbits, (WIDE_1_VAL..., true))
        test_gpu_compiles(random_item_isbits, (WIDE_1_VAL..., ()))
        test_gpu_throws("getfield", random_item_isbits, (WIDE_1_VAL..., Val(2)))
    else
        test_gpu_throws("box", random_item_isbits, (WIDE_1_VAL..., true))
        test_gpu_throws("box", random_item_isbits, (WIDE_1_VAL..., ()))
        test_gpu_throws("box", random_item_isbits, (WIDE_1_VAL..., Val(2)))
    end
    test_gpu_throws_alloc(random_item_isbits, (WIDE_1_VAL..., (), (1,)))

    # Unions are preserved by unrolled_unique up to three runtime bools, but in
    # some cases the limit can drop down to two, like with (1, 2, 3.0), or even
    # all the way down to zero, like in the case of (1, vals(2)...).
    test_gpu_compiles(runtime_num_unique, (1, 2, 3))
    test_gpu_compiles(runtime_num_unique, (1, 2.0, 3.0))
    test_gpu_compiles(runtime_num_unique, (1, Val(1)))
    test_gpu_throws_alloc(runtime_num_unique, (1, 2, 3, 4))
    test_gpu_throws_alloc(runtime_num_unique, (1, 2, 3.0))
    test_gpu_throws_alloc(runtime_num_unique, (1, 2, Val(1)))
    test_gpu_throws_alloc(runtime_num_unique, (1, vals(2)...))

    # Unions are preserved by unrolled_filter up to two runtime bools, but in
    # some cases the limit can drop down to one, like in the case of (1, 2.0).
    test_gpu_compiles(runtime_num_filtered, (1, 2))
    test_gpu_compiles(runtime_num_filtered, (1, 2, vals(20)...))
    test_gpu_throws_alloc(runtime_num_filtered, (1, 2, 3))
    test_gpu_throws_alloc(runtime_num_filtered, (1, 2, 3, vals(20)...))
    test_gpu_throws_alloc(runtime_num_filtered, (1, 2.0))
end
