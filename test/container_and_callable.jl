using Test
using JET
using StaticArrays: SVector, MVector
using UnrolledUtilities

# Regression tests for properties that elementwise equality comparisons in
# test_and_analyze.jl cannot detect: preservation of output container types,
# the number of times unrolled_split evaluates its predicate, support for
# callable non-Function predicates, and the type stability of
# unrolled_product for heterogeneous iterators without any effects
# annotations.

# A predicate that is not a Function, so it has no ! method, and that counts
# how many times it is called.
mutable struct OddCallCounter
    count::Int
end
(counter::OddCallCounter)(x) = (counter.count += 1; isodd(x))

@testset "output container preservation" begin
    bitvector = StaticBitVector{10}(isodd)

    filtered_bitvector = unrolled_filter(identity, bitvector)
    @test filtered_bitvector isa StaticBitVector
    @test Tuple(filtered_bitvector) == ntuple(Returns(true), 5)

    unique_bitvector = unrolled_unique(bitvector)
    @test unique_bitvector isa StaticBitVector
    @test Tuple(unique_bitvector) == (true, false)

    split_bitvectors = unrolled_split(identity, bitvector)
    @test split_bitvectors[1] isa StaticBitVector
    @test split_bitvectors[2] isa StaticBitVector
    @test Tuple(split_bitvectors[1]) == ntuple(Returns(true), 5)
    @test Tuple(split_bitvectors[2]) == ntuple(Returns(false), 5)

    for vector in (SVector(1, 2, 3, 4, 5), MVector(1, 2, 3, 4, 5))
        filtered_vector = unrolled_filter(isodd, vector)
        @test filtered_vector isa typeof(vector).name.wrapper
        @test Tuple(filtered_vector) == (1, 3, 5)

        unique_vector = unrolled_unique(isodd, vector)
        @test unique_vector isa typeof(vector).name.wrapper
        @test Tuple(unique_vector) == (1, 2)

        split_vectors = unrolled_split(isodd, vector)
        @test split_vectors[1] isa typeof(vector).name.wrapper
        @test split_vectors[2] isa typeof(vector).name.wrapper
        @test Tuple(split_vectors[1]) == (1, 3, 5)
        @test Tuple(split_vectors[2]) == (2, 4)
    end
end

@testset "unrolled_split predicate calls and callable structs" begin
    counter = OddCallCounter(0)
    @test unrolled_split(counter, (1, 2, 3, 4, 5)) == ((1, 3, 5), (2, 4))
    @test counter.count == 5

    # Tuples with heterogeneous item types must also evaluate the predicate
    # only once per item and remain allocation-free and type-stable.
    heterogeneous_itr = (1, 0.5, 2, Val(3), :a, (4, 5))
    is_number(x) = x isa Number
    @test unrolled_split(is_number, heterogeneous_itr) ==
          ((1, 0.5, 2), (Val(3), :a, (4, 5)))
    split_heterogeneous(itr) = unrolled_split(is_number, itr)
    split_heterogeneous_and_nothing(itr) = (split_heterogeneous(itr); nothing)
    @test_opt split_heterogeneous(heterogeneous_itr)
    split_heterogeneous_and_nothing(heterogeneous_itr)
    @test (@allocated split_heterogeneous_and_nothing(heterogeneous_itr)) == 0
end

@testset "heterogeneous unrolled_product type stability" begin
    # Type stability is asserted with JET, but allocations are not asserted
    # to be zero: products of iterators with heterogeneous item types
    # allocate their results on CPUs regardless of how unrolled_product is
    # implemented.
    heterogeneous_product(itrs...) = unrolled_product(itrs...)
    itr1 = (1, 1.0f0, Val(1))
    itr2 = StaticOneTo(3)
    itr3 = (:a, :b)
    @test length(unrolled_product(itr1, itr2, itr3)) == 18
    @test unrolled_product(itr1, itr2, itr3) ==
          Tuple(vec(collect(Iterators.product(itr1, itr2, itr3))))
    @test_opt heterogeneous_product(itr1, itr2)
    @test_opt heterogeneous_product(itr1, itr2, itr3)
end

# The results of these functions are used as type parameters downstream
# (e.g., the names in ClimaCore's Components{T, names}), so their values must
# constant-fold when their inputs are constant. Wrapping each result in Val
# makes inference fail unless the value folds. Even a single additional call
# layer can break folding on some Julia versions while leaving results equal
# and types stable, so these tests must run on every CI Julia version.
@testset "value-level constant folding for constant inputs" begin
    # The input arrives as a type parameter, mirroring how ClimaCore's
    # Components type provides its names to these functions.
    filter_val(::Val{names}) where {names} =
        Val(unrolled_filter(n -> length(n) == 1, names))
    unique_val(::Val{names}) where {names} = Val(unrolled_unique(names))
    allunique_val(::Val{names}) where {names} = Val(unrolled_allunique(names))
    split_val(::Val{names}) where {names} =
        Val(unrolled_split(n -> length(n) == 1, names))
    take_val(::Val{names}) where {names} = Val(unrolled_take(names, Val(2)))
    drop_val(::Val{names}) where {names} = Val(unrolled_drop(names, Val(2)))
    v = Val(((1,), (2, 3), (1,), (2, 3), (4,)))
    @test @inferred(filter_val(v)) == Val(((1,), (1,), (4,)))
    @test @inferred(unique_val(v)) == Val(((1,), (2, 3), (4,)))
    @test @inferred(allunique_val(v)) == Val(false)
    @test @inferred(split_val(v)) == Val((((1,), (1,), (4,)), ((2, 3), (2, 3))))
    @test @inferred(take_val(v)) == Val(((1,), (2, 3)))
    @test @inferred(drop_val(v)) == Val(((1,), (2, 3), (4,)))
end

@testset "Init positional API" begin
    itr = (1, 2, 3, 4, 5)

    # unrolled_reduce with Init
    @test unrolled_reduce(+, itr, Init(10)) == 25
    @test unrolled_reduce(+, itr, 10) == 25  # plain positional still works
    @test unrolled_reduce(+, (), Init(42)) == 42  # empty itr returns init
    @test @inferred(unrolled_reduce(+, itr, Init(0))) == 15

    # unrolled_mapreduce with Init (positional, ahead of varargs)
    @test unrolled_mapreduce(x -> x^2, +, Init(100), itr) == 155
    @test unrolled_mapreduce(x -> x^2, +, itr; init = 100) == 155  # kwarg compat
    @test @inferred(unrolled_mapreduce(x -> x^2, +, Init(0), itr)) == 55

    # unrolled_mapreduce with Init and multiple iterators
    @test unrolled_mapreduce(+, +, Init(100), itr, itr) == 130

    # unrolled_accumulate with Init
    @test unrolled_accumulate(+, itr, Init(10)) == (11, 13, 16, 20, 25)
    @test unrolled_accumulate(+, itr, 10) == (11, 13, 16, 20, 25)
    @test @inferred(unrolled_accumulate(+, itr, Init(0))) == (1, 3, 6, 10, 15)

    # unrolled_sum positional (3-arg: f, itr, init)
    # Note: unrolled_sum/prod only use init if the iterator is empty!
    @test unrolled_sum(identity, itr, 100) == 15
    @test unrolled_sum(x -> x^2, itr, 100) == 55
    @test unrolled_sum(identity, (), 42) == 42
    @test @inferred(unrolled_sum(identity, itr, 0)) == 15

    # unrolled_prod positional (3-arg: f, itr, init)
    @test unrolled_prod(identity, itr, 10) == 120
    @test unrolled_prod(x -> x + 1, itr, 1) == 720
    @test unrolled_prod(identity, (), 42) == 42
    @test @inferred(unrolled_prod(identity, itr, 1)) == 120

    # Verify kwarg shims still work identically
    @test unrolled_sum(itr; init = 100) == 15
    @test unrolled_prod(itr; init = 10) == 120
    @test unrolled_reduce(+, itr; init = 10) == 25
    @test unrolled_accumulate(+, itr; init = 10) == (11, 13, 16, 20, 25)

    # An Init wrapper and a bare value are interchangeable in every function
    # that accepts an init value, including the ones that only use the init
    # value when the iterator is empty.
    @test unrolled_sum(identity, (), Init(42)) == 42
    @test unrolled_prod(identity, (), Init(42)) == 42
    @test unrolled_sum(identity, itr, Init(100)) == 15
    @test unrolled_prod(identity, itr, Init(10)) == 120
    bv = StaticBitVector{4}(Returns(false))
    @test unrolled_accumulate(|, bv, Init(true)) ==
          unrolled_accumulate(|, bv, true)

    # Type stability and zero allocations for Init paths
    reduce_init(t) = unrolled_reduce(+, t, Init(0))
    mapreduce_init(t) = unrolled_mapreduce(identity, +, Init(0), t)
    @test @inferred(reduce_init(itr)) == 15
    @test @inferred(mapreduce_init(itr)) == 15

    reduce_init_nothing(t) = (reduce_init(t); nothing)
    reduce_init_nothing(itr)
    @test (@allocated reduce_init_nothing(itr)) == 0
end

@testset "StaticBitVector accumulate fix (H3)" begin
    # This would throw MethodError before the fix due to the stale 4th
    # positional `first` argument to unrolled_accumulate.
    bv = StaticBitVector{8}(isodd)
    cumor = unrolled_accumulate(|, bv)
    @test cumor isa StaticBitVector
    # cumulative OR of [true, false, true, false, true, false, true, false]
    # → every position from index 1 onward should be true.
    @test all(Tuple(cumor))

    cumand = unrolled_accumulate(&, bv)
    @test cumand isa StaticBitVector
    # cumulative AND: first is true, then true & false = false, stays false.
    @test Tuple(cumand) ==
          (true, false, false, false, false, false, false, false)

    # With Init
    bv_short = StaticBitVector{4}(Returns(false))
    cum_init = unrolled_accumulate(|, bv_short, true)
    @test cum_init isa StaticBitVector
    # Init=true, OR with false → all true.
    @test all(Tuple(cum_init))
end
