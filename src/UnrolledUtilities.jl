module UnrolledUtilities

export unrolled_any,
    unrolled_all,
    unrolled_foreach,
    unrolled_map,
    unrolled_reduce,
    unrolled_mapreduce,
    unrolled_zip,
    unrolled_enumerate,
    unrolled_in,
    unrolled_unique,
    unrolled_filter,
    unrolled_split,
    unrolled_flatten,
    unrolled_flatmap,
    unrolled_product,
    unrolled_applyat,
    unrolled_take,
    unrolled_drop

inferred_length(::Type{<:NTuple{N, Any}}) where {N} = N
# We could also add support for statically-sized iterators that are not Tuples.

f_exprs(itr_type) = (:(f(itr[$n])) for n in 1:inferred_length(itr_type))
@inline @generated unrolled_any(f, itr) = Expr(:||, f_exprs(itr)...)
@inline @generated unrolled_all(f, itr) = Expr(:&&, f_exprs(itr)...)

function zipped_f_exprs(itr_types)
    L = length(itr_types)
    L == 0 && error("unrolled functions need at least one iterator as input")
    N = minimum(inferred_length, itr_types)
    return (:(f($((:(itrs[$l][$n]) for l in 1:L)...))) for n in 1:N)
end
@inline @generated unrolled_foreach(f, itrs...) =
    Expr(:block, zipped_f_exprs(itrs)..., nothing)
@inline @generated unrolled_map(f, itrs...) =
    Expr(:tuple, zipped_f_exprs(itrs)...)

function nested_op_expr(itr_type)
    N = inferred_length(itr_type)
    N == 0 && error("unrolled_reduce needs an `init` value for empty iterators")
    item_exprs = (:(itr[$n]) for n in 1:N)
    return reduce((expr1, expr2) -> :(op($expr1, $expr2)), item_exprs)
end
@inline @generated unrolled_reduce_without_init(op, itr) = nested_op_expr(itr)

struct NoInit end
@inline unrolled_reduce(op, itr; init = NoInit()) =
    unrolled_reduce_without_init(op, init isa NoInit ? itr : (init, itr...))

@inline unrolled_mapreduce(f, op, itrs...; init = NoInit()) =
    unrolled_reduce(op, unrolled_map(f, itrs...); init)

@inline unrolled_zip(itrs...) = unrolled_map(tuple, itrs...)

@inline unrolled_enumerate(itrs...) =
    unrolled_zip(ntuple(identity, Val(length(itrs[1]))), itrs...)

@inline unrolled_in(item, itr) = unrolled_any(Base.Fix1(===, item), itr)
# Using === instead of == or isequal improves type stability for singletons.

@inline unrolled_unique(itr) =
    unrolled_reduce(itr; init = ()) do unique_items, item
        @inline
        unrolled_in(item, unique_items) ? unique_items : (unique_items..., item)
    end

@inline unrolled_filter(f, itr) =
    unrolled_reduce(itr; init = ()) do filtered_items, item
        @inline
        f(item) ? (filtered_items..., item) : filtered_items
    end

@inline unrolled_split(f, itr) =
    unrolled_reduce(itr; init = ((), ())) do (f_items, not_f_items), item
        @inline
        f(item) ? ((f_items..., item), not_f_items) :
        (f_items, (not_f_items..., item))
    end

@inline unrolled_flatten(itr) =
    unrolled_reduce((item1, item2) -> (item1..., item2...), itr; init = ())

@inline unrolled_flatmap(f, itrs...) =
    unrolled_flatten(unrolled_map(f, itrs...))

@inline unrolled_product(itrs...) =
    unrolled_reduce(itrs; init = ((),)) do product_itr, itr
        @inline
        unrolled_flatmap(itr) do item
            @inline
            unrolled_map(product_tuple -> (product_tuple..., item), product_itr)
        end
    end

@inline unrolled_applyat(f, n, itrs...) = unrolled_foreach(
    (i, items...) -> i == n && f(items...),
    unrolled_enumerate(itrs...),
)

@inline unrolled_take(itr, ::Val{N}) where {N} = ntuple(i -> itr[i], Val(N))
@inline unrolled_drop(itr, ::Val{N}) where {N} =
    ntuple(i -> itr[N + i], Val(length(itr) - N))
# When its second argument is a Val, ntuple is unrolled via Base.@ntuple.

@static if hasfield(Method, :recursion_relation)
    # Remove recursion limits for functions whose arguments are also functions.
    for func in (
        unrolled_any,
        unrolled_all,
        unrolled_foreach,
        unrolled_map,
        unrolled_reduce_without_init,
        unrolled_reduce,
        unrolled_mapreduce,
        unrolled_filter,
        unrolled_split,
        unrolled_flatmap,
        unrolled_applyat,
    )
        for method in methods(func)
            method.recursion_relation = (_...) -> true
        end
    end
end

end
