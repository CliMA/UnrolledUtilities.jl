@inline @generated _gen_unrolled_any(::Val{N}, f, itr) where {N} =
    Expr(:||, (:(f(generic_getindex(itr, $n))) for n in 1:N)...)
@inline gen_unrolled_any(f, itr) = _gen_unrolled_any(Val(length(itr)), f, itr)

@inline @generated _gen_unrolled_all(::Val{N}, f, itr) where {N} =
    Expr(:&&, (:(f(generic_getindex(itr, $n))) for n in 1:N)...)
@inline gen_unrolled_all(f, itr) = _gen_unrolled_all(Val(length(itr)), f, itr)

@inline @generated _gen_unrolled_foreach(::Val{N}, f, itr) where {N} =
    Expr(:block, (:(f(generic_getindex(itr, $n))) for n in 1:N)..., nothing)
@inline gen_unrolled_foreach(f, itr) =
    _gen_unrolled_foreach(Val(length(itr)), f, itr)

@inline @generated _gen_unrolled_map(::Val{N}, f, itr) where {N} =
    Expr(:tuple, (:(f(generic_getindex(itr, $n))) for n in 1:N)...)
@inline gen_unrolled_map(f, itr) = _gen_unrolled_map(Val(length(itr)), f, itr)

@inline @generated _gen_unrolled_applyat(::Val{N}, f, n, itr) where {N} = Expr(
    :block,
    (:(n == $n && return f(generic_getindex(itr, $n))) for n in 1:N)...,
    :(unrolled_applyat_bounds_error()),
) # This block gets optimized into a switch instruction during LLVM codegen.
@inline gen_unrolled_applyat(f, n, itr) =
    _gen_unrolled_applyat(Val(length(itr)), f, n, itr)

@inline @generated _gen_unrolled_reduce(::Val{N}, op, itr, init) where {N} =
    foldl(
        init <: NoInit ? (2:N) : (1:N);
        init = init <: NoInit ? :(generic_getindex(itr, 1)) : :init,
    ) do prev_op_expr, n
        :(op($prev_op_expr, generic_getindex(itr, $n)))
    end # Use foldl instead of reduce to guarantee left associativity.
@inline gen_unrolled_reduce(op, itr, init) =
    _gen_unrolled_reduce(Val(length(itr)), op, itr, init)

@inline @generated function _gen_unrolled_accumulate(
    ::Val{N},
    op,
    itr,
    init,
    transform,
) where {N}
    first_item_expr = :(generic_getindex(itr, 1))
    init_expr = init <: NoInit ? first_item_expr : :(op(init, $first_item_expr))
    transformed_exprs_and_op_exprs =
        accumulate(1:N; init = (nothing, init_expr)) do (_, prev_op_expr), n
            var = gensym()
            op_expr = :(op($var, generic_getindex(itr, $(n + 1))))
            (:($var = $prev_op_expr; transform($var)), op_expr)
        end
    return Expr(:tuple, Iterators.map(first, transformed_exprs_and_op_exprs)...)
end
@inline gen_unrolled_accumulate(op, itr, init, transform) =
    _gen_unrolled_accumulate(Val(length(itr)), op, itr, init, transform)

# TODO: The following is experimental and will likely be removed in the future.
# For some reason, combining these two methods into one (or combining them with
# the method for gen_unrolled_reduce defined above) causes compilation of the
# non-orographic gravity wave parametrization test in ClimaAtmos to hang. Even
# more bizarrely, using the assignment form of the first method definition below
# (as opposed to the function syntax used here) causes compilation to hang as
# well. This behavior has not yet been replicated in a minimal working example.
@inline @generated function val_unrolled_reduce(op, ::Val{N}, init) where {N}
    return foldl((:init, 1:N...)) do prev_op_expr, item_expr
        :(op($prev_op_expr, $item_expr))
    end
end
@inline @generated val_unrolled_reduce(op, ::Val{N}, ::NoInit) where {N} =
    foldl(1:N) do prev_op_expr, item_expr
        :(op($prev_op_expr, $item_expr))
    end
