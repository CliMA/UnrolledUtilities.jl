@inline _rec_unrolled_any(f) = false
@inline _rec_unrolled_any(f, item, items...) =
    f(item) || _rec_unrolled_any(f, items...)
@inline rec_unrolled_any(f, itr) = _rec_unrolled_any(f, itr...)

@inline _rec_unrolled_all(f) = true
@inline _rec_unrolled_all(f, item, items...) =
    f(item) && _rec_unrolled_all(f, items...)
@inline rec_unrolled_all(f, itr) = _rec_unrolled_all(f, itr...)

@inline _rec_unrolled_foreach(f) = nothing
@inline _rec_unrolled_foreach(f, item, items...) =
    (f(item); _rec_unrolled_foreach(f, items...))
@inline rec_unrolled_foreach(f, itr) = _rec_unrolled_foreach(f, itr...)

@inline _rec_unrolled_map(f) = ()
@inline _rec_unrolled_map(f, item, items...) =
    (f(item), _rec_unrolled_map(f, items...)...)
@inline rec_unrolled_map(f, itr) = _rec_unrolled_map(f, itr...)

@inline _rec_unrolled_applyat(f, offset_n) = unrolled_applyat_bounds_error()
@inline _rec_unrolled_applyat(f, offset_n, item, items...) =
    offset_n == 1 ? f(item) : _rec_unrolled_applyat(f, offset_n - 1, items...)
@inline rec_unrolled_applyat(f, n, itr) = _rec_unrolled_applyat(f, n, itr...)

@inline _rec_unrolled_reduce(op, prev_value) = prev_value
@inline _rec_unrolled_reduce(op, prev_value, item, items...) =
    _rec_unrolled_reduce(op, op(prev_value, item), items...)
@inline rec_unrolled_reduce(op, itr, init) =
    init isa NoInit ? _rec_unrolled_reduce(op, itr...) :
    _rec_unrolled_reduce(op, init, itr...)

@inline _rec_unrolled_accumulate(op, transform, prev_value) =
    (transform(prev_value),)
@inline _rec_unrolled_accumulate(op, transform, prev_value, item, items...) = (
    transform(prev_value),
    _rec_unrolled_accumulate(op, transform, op(prev_value, item), items...)...,
)
@inline rec_unrolled_accumulate(op, itr, init, transform) =
    isempty(itr) ? () :
    init isa NoInit ? _rec_unrolled_accumulate(op, transform, itr...) :
    _rec_unrolled_accumulate(
        op,
        transform,
        op(init, generic_getindex(itr, 1)),
        unrolled_drop(itr, Val(1))...,
    )
