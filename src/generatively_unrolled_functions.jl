@generated _gen_unrolled_map(::Val{N}, f, itr) where {N} = quote
    @inline
    return Base.Cartesian.@ntuple $N n -> f(generic_getindex(itr, n))
end
@inline gen_unrolled_map(f, itr) = _gen_unrolled_map(Val(length(itr)), f, itr)

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

@generated _gen_unrolled_reduce(::Val{N}, op, itr, init) where {N} = quote
    @inline
    $N == 0 && return init
    first_itr_item = generic_getindex(itr, 1)
    value_1 = init isa NoInit ? first_itr_item : op(init, first_itr_item)
    Base.Cartesian.@nexprs $(N - 1) n ->
        (value_{n + 1} = op(value_n, generic_getindex(itr, n + 1)))
    return $(Symbol(:value_, N))
end
@inline gen_unrolled_reduce(op, itr, init) =
    _gen_unrolled_reduce(Val(length(itr)), op, itr, init)

@generated _gen_unrolled_accumulate(::Val{N}, op, itr, init) where {N} = quote
    @inline
    $N == 0 && return ()
    first_itr_item = generic_getindex(itr, 1)
    value_1 = init isa NoInit ? first_itr_item : op(init, first_itr_item)
    Base.Cartesian.@nexprs $(N - 1) n ->
        (value_{n + 1} = op(value_n, generic_getindex(itr, n + 1)))
    return Base.Cartesian.@ntuple $N n -> value_n
end
@inline gen_unrolled_accumulate(op, itr, init) =
    _gen_unrolled_accumulate(Val(length(itr)), op, itr, init)

@generated _gen_unrolled_ifelse(::Val{N}, f, get_if, get_else, itr) where {N} =
    quote
        @inline
        Base.Cartesian.@nexprs $N n -> begin
            item = generic_getindex(itr, n)
            f(item) && return get_if(item)
        end
        return get_else()
    end
@inline gen_unrolled_ifelse(f, get_if, get_else, itr) =
    _gen_unrolled_ifelse(Val(length(itr)), f, get_if, get_else, itr)

@generated _gen_unrolled_ifelse2(
    ::Val{N},
    f,
    get_if,
    get_else,
    itr1,
    itr2,
) where {N} = quote
    @inline
    Base.Cartesian.@nexprs $N n -> begin
        f(generic_getindex(itr1, n)) && return get_if(generic_getindex(itr2, n))
    end
    return get_else()
end
@inline gen_unrolled_ifelse(f, get_if, get_else, itr1, itr2) =
    _gen_unrolled_ifelse2(Val(length(itr1)), f, get_if, get_else, itr1, itr2)
