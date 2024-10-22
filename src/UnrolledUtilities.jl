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

@inline unrolled_any(f::F, itr) where {F} =
    (rec_unroll(itr) ? rec_unrolled_any : gen_unrolled_any)(f, itr)
@inline unrolled_any(itr) = unrolled_any(identity, itr)

@inline unrolled_all(f::F, itr) where {F} =
    (rec_unroll(itr) ? rec_unrolled_all : gen_unrolled_all)(f, itr)
@inline unrolled_all(itr) = unrolled_all(identity, itr)

@inline unrolled_foreach(f::F, itr) where {F} =
    (rec_unroll(itr) ? rec_unrolled_foreach : gen_unrolled_foreach)(f, itr)
@inline unrolled_foreach(f, itrs...) = unrolled_foreach(splat(f), zip(itrs...))

@inline unrolled_map_into_tuple(f::F, itr) where {F} =
    (rec_unroll(itr) ? rec_unrolled_map : gen_unrolled_map)(f, itr)
@inline unrolled_map_into(output_type, f::F, itr) where {F} =
    constructor_from_tuple(output_type)(unrolled_map_into_tuple(f, itr))
@inline unrolled_map(f::F, itr) where {F} =
    unrolled_map_into(inferred_output_type(Iterators.map(f, itr)), f, itr)
@inline unrolled_map(f::F, itrs...) where {F} =
    unrolled_map(splat(f), zip(itrs...))

@inline unrolled_applyat(f::F, n, itr) where {F} =
    (rec_unroll(itr) ? rec_unrolled_applyat : gen_unrolled_applyat)(f, n, itr)
@inline unrolled_applyat(f::F, n, itrs...) where {F} =
    unrolled_applyat(splat(f), n, zip(itrs...))
@inline unrolled_applyat_bounds_error() =
    error("unrolled_applyat has detected an out-of-bounds index")

@inline unrolled_reduce(op::O, itr, init) where {O} =
    isempty(itr) && init isa NoInit ?
    error("unrolled_reduce requires an init value for empty iterators") :
    (rec_unroll(itr) ? rec_unrolled_reduce : gen_unrolled_reduce)(op, itr, init)
@inline unrolled_reduce(op::O, itr; init = NoInit()) where {O} =
    unrolled_reduce(op, itr, init)

# TODO: Figure out why unrolled_reduce(op, Val(N), init) compiles faster than
# unrolled_reduce(op, StaticOneTo(N), init) for the non-orographic gravity wave
# parametrization test in ClimaAtmos, to the point where the StaticOneTo version
# completely hangs while the Val version compiles in only a few seconds.
@inline unrolled_reduce(op::O, val_N::Val, init) where {O} =
    val_N isa Val{0} && init isa NoInit ?
    error("unrolled_reduce requires an init value for Val(0)") :
    val_unrolled_reduce(op, val_N, init)

@inline unrolled_mapreduce(f::F, op::O, itrs...; init = NoInit()) where {F, O} =
    unrolled_reduce(op, unrolled_map(f, itrs...), init)

@inline unrolled_accumulate_into_tuple(
    op::O,
    itr,
    init,
    transform::T,
) where {O, T} =
    (rec_unroll(itr) ? rec_unrolled_accumulate : gen_unrolled_accumulate)(
        op,
        itr,
        init,
        transform,
    )
@inline unrolled_accumulate_into(
    output_type,
    op::O,
    itr,
    init,
    transform::T,
) where {O, T} = constructor_from_tuple(output_type)(
    unrolled_accumulate_into_tuple(op, itr, init, transform),
)
@inline unrolled_accumulate(op::O, itr, init, transform::T) where {O, T} =
    unrolled_accumulate_into(
        accumulate_output_type(op, itr, init, transform),
        op,
        itr,
        init,
        transform,
    )
@inline unrolled_accumulate(
    op::O,
    itr;
    init = NoInit(),
    transform::T = identity,
) where {O, T} = unrolled_accumulate(op, itr, init, transform)

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
    unrolled_reduce(itr, inferred_empty(itr)) do unique_items, item
        @inline
        unrolled_in(item, unique_items) ? unique_items :
        unrolled_push(unique_items, item)
    end

@inline unrolled_filter(f::F, itr) where {F} =
    unrolled_reduce(itr, inferred_empty(itr)) do items_with_true_f, item
        @inline
        f(item) ? unrolled_push(items_with_true_f, item) : items_with_true_f
    end

@inline unrolled_split(f::F, itr) where {F} =
    unrolled_reduce(
        itr,
        (inferred_empty(itr), inferred_empty(itr)),
    ) do (items_with_true_f, items_with_false_f), item
        @inline
        f(item) ? (unrolled_push(items_with_true_f, item), items_with_false_f) :
        (items_with_true_f, unrolled_push(items_with_false_f, item))
    end

@inline unrolled_flatten(itr) =
    unrolled_reduce(unrolled_append, itr, promoted_empty(itr))

@inline unrolled_flatmap(f::F, itrs...) where {F} =
    unrolled_flatten(unrolled_map(f, itrs...))

@inline unrolled_product(itrs...) =
    unrolled_reduce(itrs, (promoted_empty(itrs),)) do product_itr, itr
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

# Remove the default recursion limit from every function defined in this module.
@static if hasfield(Method, :recursion_relation)
    module_names = names(@__MODULE__; all = true)
    module_values = map(Base.Fix1(getproperty, @__MODULE__), module_names)
    module_functions = filter(Base.Fix2(isa, Function), module_values)
    for f in module_functions, method in methods(f)
        method.recursion_relation = Returns(true)
    end
end

end
