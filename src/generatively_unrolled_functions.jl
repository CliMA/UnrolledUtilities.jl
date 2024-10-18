@generated _gen_unrolled_any(::Val{N}, f, itr) where {N} = quote
    @inline
    return Base.Cartesian.@nany $N n -> f(generic_getindex(itr, n))
end
@inline gen_unrolled_any(f, itr) = _gen_unrolled_any(Val(length(itr)), f, itr)

@generated _gen_unrolled_all(::Val{N}, f, itr) where {N} = quote
    @inline
    return Base.Cartesian.@nall $N n -> f(generic_getindex(itr, n))
end
@inline gen_unrolled_all(f, itr) = _gen_unrolled_all(Val(length(itr)), f, itr)

@generated _gen_unrolled_foreach(::Val{N}, f, itr) where {N} = quote
    @inline
    Base.Cartesian.@nexprs $N n -> f(generic_getindex(itr, n))
    return nothing
end
@inline gen_unrolled_foreach(f, itr) =
    _gen_unrolled_foreach(Val(length(itr)), f, itr)

@generated _gen_unrolled_map(::Val{N}, f, itr) where {N} = quote
    @inline
    return Base.Cartesian.@ntuple $N n -> f(generic_getindex(itr, n))
end
@inline gen_unrolled_map(f, itr) = _gen_unrolled_map(Val(length(itr)), f, itr)

@generated _gen_unrolled_applyat(::Val{N}, f, n′, itr) where {N} = quote
    @inline
    Base.Cartesian.@nexprs $N n ->
        (n′ == n && return f(generic_getindex(itr, n)))
    unrolled_applyat_bounds_error()
end # This is optimized into a switch instruction during LLVM code generation.
@inline gen_unrolled_applyat(f, n, itr) =
    _gen_unrolled_applyat(Val(length(itr)), f, n, itr)

@generated _gen_unrolled_reduce(::Val{N}, op, itr, init) where {N} = quote
    @inline
    value_0 = init
    $N == 0 && return value_0
    return Base.Cartesian.@nexprs $N n ->
        (value_n = op(value_{n - 1}, generic_getindex(itr, n)))
end
@generated _gen_unrolled_reduce(::Val{N}, op, itr, ::NoInit) where {N} = quote
    @inline
    value_1 = generic_getindex(itr, 1)
    $N == 1 && return value_1
    return Base.Cartesian.@nexprs $(N - 1) n ->
        (value_{n + 1} = op(value_n, generic_getindex(itr, n + 1)))
end
@inline gen_unrolled_reduce(op, itr, init) =
    _gen_unrolled_reduce(Val(length(itr)), op, itr, init)

@generated _gen_unrolled_accumulate(
    ::Val{N},
    op,
    itr,
    init,
    transform,
) where {N} = quote
    @inline
    $N == 0 && return ()
    first_itr_item = generic_getindex(itr, 1)
    value_1 = init isa NoInit ? first_itr_item : op(init, first_itr_item)
    Base.Cartesian.@nexprs $(N - 1) n ->
        (value_{n + 1} = op(value_n, generic_getindex(itr, n + 1)))
    return Base.Cartesian.@ntuple $N n -> transform(value_n)
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
