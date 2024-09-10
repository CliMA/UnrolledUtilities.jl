@generated _gen_unrolled_any(::Val{N}, f, itr) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    Expr(:||, (:(f(generic_getindex(itr, $n))) for n in 1:N)...),
)
@inline gen_unrolled_any(f, itr) = _gen_unrolled_any(Val(length(itr)), f, itr)

@generated _gen_unrolled_all(::Val{N}, f, itr) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    Expr(:&&, (:(f(generic_getindex(itr, $n))) for n in 1:N)...),
)
@inline gen_unrolled_all(f, itr) = _gen_unrolled_all(Val(length(itr)), f, itr)

@generated _gen_unrolled_foreach(::Val{N}, f, itr) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    (:(f(generic_getindex(itr, $n))) for n in 1:N)...,
    nothing,
)
@inline gen_unrolled_foreach(f, itr) =
    _gen_unrolled_foreach(Val(length(itr)), f, itr)

@generated _gen_unrolled_map(::Val{N}, f, itr) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    Expr(:tuple, (:(f(generic_getindex(itr, $n))) for n in 1:N)...),
)
@inline gen_unrolled_map(f, itr) = _gen_unrolled_map(Val(length(itr)), f, itr)

@generated _gen_unrolled_applyat(::Val{N}, f, n, itr) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    (:(n == $n && return f(generic_getindex(itr, $n))) for n in 1:N)...,
    :(unrolled_applyat_bounds_error()),
) # This block gets optimized into a switch instruction during LLVM codegen.
@inline gen_unrolled_applyat(f, n, itr) =
    _gen_unrolled_applyat(Val(length(itr)), f, n, itr)

@generated _gen_unrolled_reduce(::Val{N}, op, itr, init) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    foldl(
        (op_expr, n) -> :(op($op_expr, generic_getindex(itr, $n))),
        (init <: NoInit ? 2 : 1):N;
        init = init <: NoInit ? :(generic_getindex(itr, 1)) : :init,
    ), # Use foldl instead of reduce to guarantee left associativity.
)
@inline gen_unrolled_reduce(op, itr, init) =
    _gen_unrolled_reduce(Val(length(itr)), op, itr, init)

@generated function _gen_unrolled_accumulate(
    ::Val{N},
    op,
    itr,
    init,
    transform,
) where {N}
    first_item_expr = :(generic_getindex(itr, 1))
    init_expr = init <: NoInit ? first_item_expr : :(op(init, $first_item_expr))
    transformed_exprs_and_op_exprs =
        accumulate(1:N; init = (nothing, init_expr)) do (_, op_expr), n
            var = gensym()
            next_op_expr = :(op($var, generic_getindex(itr, $(n + 1))))
            (:($var = $op_expr; transform($var)), next_op_expr)
        end
    return Expr(
        :block,
        Expr(:meta, :inline),
        Expr(:tuple, Iterators.map(first, transformed_exprs_and_op_exprs)...),
    )
end
@inline gen_unrolled_accumulate(op, itr, init, transform) =
    _gen_unrolled_accumulate(Val(length(itr)), op, itr, init, transform)

# TODO: The following is experimental and will likely be removed in the future.
# For some reason, combining these two methods into one (or combining them with
# the method for gen_unrolled_reduce defined above) causes compilation of the
# non-orographic gravity wave parametrization test in ClimaAtmos to hang.
# Wrapping the first method's result in a block and adding an inline annotation
# also causes compilation to hang. Even using the assignment form of the first
# method definition below (as opposed to the function syntax used here) causes
# it to hang. This has not yet been replicated in a minimal working example.
@generated function val_unrolled_reduce(op, ::Val{N}, init) where {N}
    return foldl((:init, 1:N...)) do prev_op_expr, item_expr
        :(op($prev_op_expr, $item_expr))
    end
end
@generated val_unrolled_reduce(op, ::Val{N}, ::NoInit) where {N} = Expr(
    :block,
    Expr(:meta, :inline),
    foldl((op_expr, item_expr) -> :(op($op_expr, $item_expr)), 1:N),
)
