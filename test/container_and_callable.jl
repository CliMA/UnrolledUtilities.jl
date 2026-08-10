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
