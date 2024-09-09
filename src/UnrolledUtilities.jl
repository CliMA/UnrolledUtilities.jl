module UnrolledUtilities

export unrolled_any,
    unrolled_all,
    unrolled_foreach,
    unrolled_map,
    unrolled_applyat,
    unrolled_reduce,
    unrolled_mapreduce,
    unrolled_accumulate,
    unrolled_push,
    unrolled_append,
    unrolled_take,
    unrolled_drop,
    unrolled_in,
    unrolled_unique,
    unrolled_filter,
    unrolled_split,
    unrolled_flatten,
    unrolled_flatmap,
    unrolled_product,
    StaticOneTo,
    StaticBitVector

struct NoInit end # Analogue of Base._InitialValue for reduction/accumulation.

include("unrollable_iterator_interface.jl")
include("recursively_unrolled_functions.jl")
include("generatively_unrolled_functions.jl")

@inline unrolled_any(f, itr) =
    (rec_unroll(itr) ? rec_unrolled_any : gen_unrolled_any)(f, itr)
@inline unrolled_any(itr) = unrolled_any(identity, itr)

@inline unrolled_all(f, itr) =
    (rec_unroll(itr) ? rec_unrolled_all : gen_unrolled_all)(f, itr)
@inline unrolled_all(itr) = unrolled_all(identity, itr)

@inline unrolled_foreach(f, itr) =
    (rec_unroll(itr) ? rec_unrolled_foreach : gen_unrolled_foreach)(f, itr)
@inline unrolled_foreach(f, itrs...) = unrolled_foreach(splat(f), zip(itrs...))

@inline unrolled_map_into_tuple(f, itr) =
    (rec_unroll(itr) ? rec_unrolled_map : gen_unrolled_map)(f, itr)
@inline unrolled_map_into(output_type, f, itr) =
    constructor_from_tuple(output_type)(unrolled_map_into_tuple(f, itr))
@inline unrolled_map(f, itr) =
    unrolled_map_into(inferred_output_type(Iterators.map(f, itr)), f, itr)
@inline unrolled_map(f, itrs...) = unrolled_map(splat(f), zip(itrs...))

@inline unrolled_applyat(f, n, itr) =
    (rec_unroll(itr) ? rec_unrolled_applyat : gen_unrolled_applyat)(f, n, itr)
@inline unrolled_applyat(f, n, itrs...) =
    unrolled_applyat(splat(f), n, zip(itrs...))
@inline unrolled_applyat_bounds_error() =
    error("unrolled_applyat has detected an out-of-bounds index")

@inline unrolled_reduce(op, itr, init) =
    (rec_unroll(itr) ? rec_unrolled_reduce : gen_unrolled_reduce)(op, itr, init)
@inline unrolled_reduce(op, itr; init = NoInit()) =
    isempty(itr) && init isa NoInit ?
    error("unrolled_reduce requires an init value for empty iterators") :
    unrolled_reduce(op, itr, init)

# TODO: Figure out why unrolled_reduce(op, Val(N), init) compiles faster than
# unrolled_reduce(op, StaticOneTo(N), init) for the non-orographic gravity wave
# parametrization test in ClimaAtmos, to the point where the StaticOneTo version
# completely hangs while the Val version compiles in only a few seconds.
@inline unrolled_reduce(op, val_N::Val, init) =
    val_unrolled_reduce(op, val_N, init)
@inline unrolled_reduce(op, val_N::Val; init = NoInit()) =
    val_N isa Val{0} && init isa NoInit ?
    error("unrolled_reduce requires an init value for empty iterators") :
    unrolled_reduce(op, val_N, init)

@inline unrolled_mapreduce(f, op, itrs...; init = NoInit()) =
    unrolled_reduce(op, Iterators.map(f, itrs...), init)

@inline unrolled_accumulate_into_tuple(op, itr, init, transform) =
    (rec_unroll(itr) ? rec_unrolled_accumulate : gen_unrolled_accumulate)(
        op,
        itr,
        init,
        transform,
    )
@inline unrolled_accumulate_into(output_type, op, itr, init, transform) =
    constructor_from_tuple(output_type)(
        unrolled_accumulate_into_tuple(op, itr, init, transform),
    )
@inline unrolled_accumulate(op, itr; init = NoInit(), transform = identity) =
    unrolled_accumulate_into(
        accumulate_output_type(op, itr, init, transform),
        op,
        itr,
        init,
        transform,
    )

@inline unrolled_push_into(output_type, itr, item) =
    constructor_from_tuple(output_type)((itr..., item))
@inline unrolled_push(itr, item) =
    unrolled_push_into(inferred_output_type(itr), itr, item)

@inline unrolled_append_into(output_type, itr1, itr2) =
    constructor_from_tuple(output_type)((itr1..., itr2...))
@inline unrolled_append(itr1, itr2) =
    unrolled_append_into(promoted_output_type((itr1, itr2)), itr1, itr2)

@inline unrolled_take_into(output_type, itr, ::Val{N}) where {N} =
    constructor_from_tuple(output_type)(
        ntuple(Base.Fix1(generic_getindex, itr), Val(N)),
    )
@inline unrolled_take(itr, val_N) =
    unrolled_take_into(inferred_output_type(itr), itr, val_N)

@inline unrolled_drop_into(output_type, itr, ::Val{N}) where {N} =
    constructor_from_tuple(output_type)(
        ntuple(n -> generic_getindex(itr, N + n), Val(length(itr) - N)),
    )
@inline unrolled_drop(itr, val_N) =
    unrolled_drop_into(inferred_output_type(itr), itr, val_N)

@inline unrolled_in(item, itr) = unrolled_any(Base.Fix1(===, item), itr)
# Using === instead of == or isequal improves type stability for singletons.

@inline unrolled_unique(itr) =
    unrolled_reduce(itr; init = inferred_empty(itr)) do unique_items, item
        @inline
        unrolled_in(item, unique_items) ? unique_items :
        unrolled_push(unique_items, item)
    end

@inline unrolled_filter(f, itr) =
    unrolled_reduce(itr; init = inferred_empty(itr)) do items_with_true_f, item
        @inline
        f(item) ? unrolled_push(items_with_true_f, item) : items_with_true_f
    end

@inline unrolled_split(f, itr) =
    unrolled_reduce(
        itr;
        init = (inferred_empty(itr), inferred_empty(itr)),
    ) do (items_with_true_f, items_with_false_f), item
        @inline
        f(item) ? (unrolled_push(items_with_true_f, item), items_with_false_f) :
        (items_with_true_f, unrolled_push(items_with_false_f, item))
    end

@inline unrolled_flatten(itr) =
    unrolled_reduce(unrolled_append, itr; init = promoted_empty(itr))

@inline unrolled_flatmap(f, itrs...) =
    unrolled_flatten(Iterators.map(f, itrs...))

@inline unrolled_product(itrs...) =
    unrolled_reduce(itrs; init = (promoted_empty(itrs),)) do product_itr, itr
        @inline
        unrolled_flatmap(itr) do item
            @inline
            unrolled_map_into_tuple(Base.Fix2(unrolled_push, item), product_itr)
        end
    end

abstract type StaticSequence{N} end

@inline Base.length(::StaticSequence{N}) where {N} = N
@inline Base.firstindex(::StaticSequence) = 1
@inline Base.lastindex(itr::StaticSequence) = length(itr)
@inline Base.getindex(itr::StaticSequence, n::Integer) =
    generic_getindex(itr, n)
@inline Base.iterate(itr::StaticSequence, n = 1) =
    n > length(itr) ? nothing : (generic_getindex(itr, n), n + 1)

include("StaticOneTo.jl")
include("StaticBitVector.jl")

include("recursion_limits.jl") # This must be included at the end of the module.

end
