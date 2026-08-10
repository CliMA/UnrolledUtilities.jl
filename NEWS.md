# NEWS

## v0.2.0

### Breaking changes

- Julia compat floor raised from 1.9 to 1.10. Julia 1.9 was never tested in CI.

### New features

- **`Init{V}` wrapper type** (exported): enables passing reduction/accumulation
  initial values positionally through varargs, avoiding `Core.kwcall` which
  fails under GPU compilation. Use `Init(value)` wherever you would write
  `; init = value` in a keyword-argument call.

  ```julia
  # Keyword form (host-side only):
  unrolled_mapreduce(f, op, itr; init = v)

  # Positional form (GPU-safe):
  unrolled_mapreduce(f, op, Init(v), itr)
  ```

- **Positional `unrolled_mapreduce(f, op, init::Init, itrs...)`**: dispatches
  the `Init` wrapper ahead of varargs, so `init` can be passed without kwargs
  even when multiple iterators are provided.

- **Positional `unrolled_sum(f, itr, init)` / `unrolled_prod(f, itr, init)`**:
  the 3-arg forms avoid `Core.kwcall` on GPU paths. Note: there is no 2-arg
  `(itr, init)` positional form because it is ambiguous with `(f, itr)`. The
  keyword forms remain as host-side convenience shims.

- **Positional `unrolled_reduce(op, itr, init)`** and
  **`unrolled_accumulate(op, itr, init)`** are now documented API (they were
  already functional but undocumented).

- **Every internal reduction passes its init value positionally.**
  `unrolled_sum`, `unrolled_prod`, `unrolled_count`, `unrolled_cumsum`,
  `unrolled_cumprod`, `unrolled_maximum`, `unrolled_minimum`,
  `unrolled_extrema`, the `findmax`/`findmin`/`argmax`/`argmin` family, and
  `unrolled_flatten` all reduce through a positional internal method, so that
  keyword arguments only appear in the outermost user-facing methods. Results
  are unchanged.

### Breaking changes to method signatures

- **`unrolled_mapreduce` now requires at least one iterator** in its
  keyword-argument form, so that an `Init` passed on its own is unambiguous.
  Calling `unrolled_mapreduce(f, op)` with no iterators now throws a
  `MethodError`.

### Bug fixes

- Fixed latent `MethodError` in `StaticBitVector`'s `unrolled_accumulate_into`:
  an extraneous 4th positional argument (`first`) was removed. Bool-accumulating `StaticBitVector` outputs now work
  correctly.

- **`unrolled_unique` and `unrolled_allunique` compile for GPUs again.** They
  index their iterators with a tuple of `Val`s rather than with a range of
  `Int`s, so the indices live in the type domain and the comparisons fold
  without relying on constant propagation. Note that uniqueness must be
  determined by type (e.g. `unrolled_unique(typeof, itr)`) to be usable in a
  kernel: when it is determined by value, the number of unique items depends
  on the values, so the type of the result does too.

### Testing

- **The kernel-safety contract is now asserted** (`test/kernel_safety.jl`).
  Every exported unrolled function is called on an iterator with 33 items of 13
  distinct types, and is required to be type-stable, allocation-free, and
  inferred to return a concrete type; no method in the package may contain a
  call to `Core.kwcall`. A companion job (`.buildkite/pipeline.yml`,
  `test/gpu/kernels.jl`) compiles GPU kernels over the same patterns, since GPU
  compilation is the only place where these limits are fully enforced.

## v0.1.10

This release predates this file; the entry below was added retroactively.

- `unrolled_filter`, `unrolled_split`, and `unrolled_unique` select items by
  flattening a `Tuple` of empty or singleton `Tuple`s, rather than by pushing
  items into an accumulator whose type changes on every step. A pushing
  reduction can trigger inference's recursion-widening heuristics for long or
  complexly typed iterators, which makes GPU compilation fail with an
  `InvalidIRError`. Output container types are preserved, `unrolled_split`
  evaluates its predicate once per item and accepts predicates that are not
  `Function`s, and the results of all three constant-fold for constant inputs,
  so that they can be used as type parameters.
- `unrolled_product` precomputes cumulative lengths instead of slicing its
  iterators inside closures, which would require constant propagation that
  fails on Julia 1.11.
- `PrettyTables` is pinned to v2 for the test and documentation table printers.
