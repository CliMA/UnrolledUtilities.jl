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

    @test unrolled_take(StaticOneTo(5), Val(3)) === StaticOneTo(3)
    @test_throws BoundsError unrolled_take(StaticOneTo(5), Val(6))
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

@testset "StaticBitVector correctness and promotion" begin
    counter_bv = OddCallCounter(0)
    @test Tuple(StaticBitVector{5}(counter_bv)) ==
          (true, false, true, false, true)
    @test counter_bv.count == 5

    bv3 = StaticBitVector{3}(false)
    @test Tuple(unrolled_insert(bv3, true, Val(1))) ==
          (true, false, false, false)
    @test Tuple(unrolled_insert(bv3, true, Val(2))) ==
          (false, true, false, false)
    @test Tuple(unrolled_insert(bv3, true, Val(4))) ==
          (false, false, false, true)
    @test_throws BoundsError unrolled_insert(bv3, true, Val(0))
    @test_throws BoundsError unrolled_insert(bv3, true, Val(5))

    bv5 = StaticBitVector{5}(false)
    bv0 = unrolled_drop(bv5, Val(5))
    @test length(bv0.ints) == 0
    @test Tuple(unrolled_push(bv0, true)) == (true,)

    bv9 = StaticBitVector{9}(false)
    bv8 = unrolled_drop(bv9, Val(1))
    @test length(bv8.ints) == 1
    @test Tuple(unrolled_push(bv8, true))[9] == true

    # Lazy wrappers and mixed promotions
    bv_rev = Iterators.reverse(StaticBitVector{4}(isodd))
    @test Tuple(unrolled_take(bv_rev, Val(2))) == (false, true)
    @test Tuple(unrolled_drop(bv_rev, Val(2))) == (false, true)
    @test Tuple(unrolled_push(bv_rev, true)) == (false, true, false, true, true)
    @test Tuple(unrolled_append(StaticBitVector{2}(true), (false, true))) ==
          (true, true, false, true)
    @test unrolled_append(StaticBitVector{2}(true), (10, 20)) ==
          (true, true, 10, 20)
    @test unrolled_append(StaticBitVector{2}(true), (true, 42)) ===
          (true, true, true, 42)
    @test unrolled_push(StaticBitVector{2}(true), 42) == (true, true, 42)
    @test unrolled_setindex(StaticBitVector{2}(true), 42, Val(1)) == (42, true)
    @test unrolled_insert(StaticBitVector{2}(true), 42, Val(2)) ==
          (true, 42, true)
    @test unrolled_accumulate(
        ==,
        Iterators.map(Returns(42), StaticBitVector{2}(true)),
    ) == (42, true)
end

@testset "NamedTuple operations and promotion" begin
    nt = (a = 1, b = 2, c = 3)
    @test unrolled_allequal(nt) == false
    @test unrolled_allequal((a = 1, b = 1)) == true
    @test unrolled_take(nt, Val(2)) === (a = 1, b = 2)
    @test unrolled_drop(nt, Val(1)) === (b = 2, c = 3)
    @test unrolled_setindex(nt, 10, Val(2)) === (a = 1, b = 10, c = 3)
    @test unrolled_push(nt, 4) === (1, 2, 3, 4)
    @test unrolled_insert(nt, 10, Val(2)) === (1, 10, 2, 3)
    @test unrolled_append(nt, (d = 4,)) === (1, 2, 3, 4)
    @test unrolled_append(nt, (4, 5)) === (1, 2, 3, 4, 5)
    @test unrolled_filter(isodd, nt) === (1, 3)
    @test unrolled_filter(Returns(true), nt) === nt
    @test unrolled_unique((a = 1, b = 1, c = 2)) === (1, 2)
    @test unrolled_split(isodd, nt) === ((1, 3), (2,))
    @test unrolled_flatten((nt,)) === nt
    @test length(unrolled_product((a = 1, b = 2), (c = 3, d = 4))) == 4
    @test unrolled_partition((a = 1, b = 2, c = 3, d = 4, e = 5), Val(2)) ===
          ((a = 1, b = 2), (c = 3, d = 4), (e = 5,))
    nt_type = NamedTuple{(:a,), Tuple{Float64}}
    @test UnrolledUtilities.constructor_from_tuple(nt_type)((1,)) === (a = 1.0,)
end

@testset "reductions, stateful callables, and NaN handling" begin
    @test unrolled_count((true,)) === 1
    @test unrolled_count((false,)) === 0
    @test unrolled_count(isodd, (1,)) === 1
    @test unrolled_sum((1, 2, 3); init = 10) === 16
    @test unrolled_sum(x -> 2x, (1, 2, 3); init = 10) === 22
    @test unrolled_prod((1, 2, 3); init = 10) === 60
    @test unrolled_prod(x -> 2x, (1, 2, 3); init = 10) === 480

    counter_u = OddCallCounter(0)
    @test unrolled_unique(counter_u, (1, 2, 3, 4, 5)) == (1, 2)
    @test counter_u.count == 5

    counter_au = OddCallCounter(0)
    @test unrolled_allunique(counter_au, (1, 2)) == true
    @test counter_au.count == 2

    @test unrolled_findmax((1.0, NaN, 2.0))[2] == 2
    @test unrolled_findmax((NaN, 1.0, NaN))[2] == 1
    @test unrolled_findmin((1.0, NaN, 2.0))[2] == 2
    @test unrolled_findmin((NaN, 1.0, NaN))[2] == 1
    for itr in ((NaN, missing), (missing, NaN), (1.0, NaN, missing, 2.0))
        @test isequal(unrolled_findmax(itr), findmax(itr))
        @test isequal(unrolled_findmin(itr), findmin(itr))
    end
end

@testset "unrolled_map fast paths" begin
    @test unrolled_map((x, y, z) -> x + y + z, (1, 2), (10, 20), (100, 200)) ===
          (111, 222)
    @test unrolled_map(+, (a = 1, b = 2), (a = 10, b = 20)) === (a = 11, b = 22)
    @test unrolled_map(+, SVector(1, 2, 3), SVector(10, 20, 30)) ===
          SVector(11, 22, 33)
end

@testset "word-level StaticBitVector operations" begin
    # 19 bits span 3 UInt8 words, and the unused bits of the last word are set
    # in vectors filled with true.
    bv0 = StaticBitVector{0}(true)
    @test unrolled_any(bv0) === false
    @test unrolled_any(!, bv0) === false
    @test unrolled_all(bv0) === true
    @test unrolled_all(!, bv0) === true
    @test unrolled_count(bv0) === 0
    @test unrolled_count(!, bv0) === 0
    @test unrolled_map(!, bv0) === bv0

    bv19 = StaticBitVector{19}(n -> isodd(n) || n == 18)
    bv19_tuple = Tuple(bv19)
    @test Tuple(unrolled_map(!, bv19)) === map(!, bv19_tuple)
    # === compares all words, including the unused bits of the last word.
    @test unrolled_map(!, bv19) === StaticBitVector{19}(n -> !bv19[n])
    @test unrolled_map(!, StaticBitVector{5}(isodd)) ===
          StaticBitVector{5}(iseven)
    @test unrolled_any(bv19) === any(bv19_tuple)
    @test unrolled_any(!, bv19) === any(!, bv19_tuple)
    @test unrolled_all(bv19) === all(bv19_tuple)
    @test unrolled_all(!, bv19) === all(!, bv19_tuple)
    @test unrolled_count(bv19) === count(bv19_tuple)
    @test unrolled_count(!, bv19) === count(!, bv19_tuple)
    @test unrolled_reduce(&, bv19) === reduce(&, bv19_tuple)
    @test unrolled_reduce(|, bv19) === reduce(|, bv19_tuple)
    @test unrolled_mapreduce(!, &, bv19) === mapreduce(!, &, bv19_tuple)
    @test unrolled_mapreduce(!, |, bv19) === mapreduce(!, |, bv19_tuple)

    bv19_all_true = StaticBitVector{19}(true)
    @test unrolled_all(bv19_all_true) === true
    @test unrolled_any(!, bv19_all_true) === false
    @test unrolled_count(bv19_all_true) === 19
    @test unrolled_count(!, bv19_all_true) === 0

    bv19_all_false = StaticBitVector{19}(false)
    @test unrolled_any(bv19_all_false) === false
    @test unrolled_all(!, bv19_all_false) === true
    @test unrolled_count(bv19_all_false) === 0
    @test unrolled_count(!, bv19_all_false) === 19
end
