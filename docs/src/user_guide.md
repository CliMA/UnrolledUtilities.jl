```@meta
CurrentModule = UnrolledUtilities
```

```@setup inference_test
using UnrolledUtilities, InteractiveUtils, Test
```

# When to Use UnrolledUtilities

The functions and types exported by this package tend to perform better than
their counterparts from `Base` and `Base.Iterators` in the scenarios listed
below. Additional examples and more precise measurements can be found in the
automatically generated [tables of benchmarks](comparison_tables.md).

##### Outline:

```@contents
Pages = ["user_guide.md"]
Depth = 2:3
```

## When to Use Unrolled Functions

### Long iterators

- `map` has an unstable return type for iterators with lengths greater than 32:

  ```@repl inference_test
  Test.@inferred map(one, Tuple(1:31));
  Test.@inferred map(one, Tuple(1:32));
  Test.@inferred unrolled_map(one, Tuple(1:32));
  ```

- `getindex` has an unstable return type for `Core.Const` slices of length
  `N > 10` from iterators with lengths greater than `N + 2`:

  ```@repl inference_test
  first_11(itr) = itr[1:11]
  Test.@inferred first_11(Tuple(1:13));
  Test.@inferred first_11(Tuple(1:14));
  unrolled_first_11(itr) = unrolled_take(itr, Val(11))
  Test.@inferred unrolled_first_11(Tuple(1:14));
  ```

- For benchmarks that indicate performance improvements when using unrolled
  functions with long iterators, see [Isolated Unrolled Functions](@ref)

### Iterators with elements of different types

- `in` has an intermediate type instability that triggers allocations for
  nonuniform iterators:

  ```@repl inference_test
  @allocated () in ((1, 2), (1, 2, 3))
  @allocated unrolled_in((), ((1, 2), (1, 2, 3)))
  ```

- `any`, `all`, and `foreach` have intermediate type instabilities that trigger
  allocations for nonuniform iterators with lengths greater than 32:

  ```@repl inference_test
  const nonuniform_itr_of_length_32 = (ntuple(Returns((1, 2)), 31)..., (1, 2, 3));
  const nonuniform_itr_of_length_33 = (ntuple(Returns((1, 2)), 32)..., (1, 2, 3));
  @allocated any(isempty, nonuniform_itr_of_length_32)
  @allocated any(isempty, nonuniform_itr_of_length_33)
  @allocated unrolled_any(isempty, nonuniform_itr_of_length_33)
  ```

- `getindex` has an unstable return type for nonuniform iterators when given
  non-constant (i.e., not `Core.Const`) indices, which can lead to intermediate
  type instabilities that trigger allocations:

  ```@repl inference_test
  function add_lengths(itr)
      length_sum = 0
      for n in 1:length(itr)
          length_sum += length(itr[n])
      end
  end
  add_lengths(((1, 2), (1, 2, 3))) # hide
  @allocated add_lengths(((1, 2), (1, 2, 3)))
  function unrolled_add_lengths(itr)
      length_sum = 0
      for n in 1:length(itr)
          length_sum += unrolled_applyat(length, n, itr)
      end
  end
  unrolled_add_lengths(((1, 2), (1, 2, 3))) # hide
  @allocated unrolled_add_lengths(((1, 2), (1, 2, 3)))
  ```

  !!! note "Note"
      ##### *How can `unrolled_applyat` be stable if `n` isn't a `Core.Const`?*

      For the example of `add_lengths`, the compiler must infer the return
      type of `itr[::Int64]` before it can compile the call to `length`.
      Since this return type depends on the index `n`, the compiler needs to
      insert a runtime lookup into the method table that determines which
      method of `length` to call, `length(::Tuple{Int64, Int64})` or
      `length(::Tuple{Int64, Int64, Int64})`, and this triggers allocations.

      For the example of `unrolled_add_lengths`, the compiler instead infers
      the return types of `itr[::Core.Const(1)]`, `itr[::Core.Const(2)]`,
      and so on for every index into `itr`. Then, it compiles a call to
      `length` for each of these return types, and it inserts a runtime
      [switch instruction](https://llvm.org/docs/LangRef.html#switch-instruction)
      that determines which result of `length` to return for a particular
      value of `n`. As long as `length` itself only returns one type (in this
      case, `Int64`), this ensures that `unrolled_add_lengths` has no
      intermediate type instabilities.
      
      In other words, `unrolled_applyat` combines multiple methods for `length`
      and `getindex` into a single method, replacing the inefficient method
      table lookup that switches between them with a simpler switch instruction.

  !!! tip "Tip"
      ##### *When should `getindex` be replaced with `unrolled_applyat`?*

      The specific example above could be simplified by using `mapreduce`,
      instead of using a `for`-loop in conjunction with `unrolled_applyat`:

      ```@repl
      @allocated mapreduce(length, +, ((1, 2), (1, 2, 3)))
      ```

      However, there are often situations in which it is not possible to
      replace loops with function calls, like when those loops are parallelized
      over CPU or GPU threads. Moreover, CUDA is unable to compile any kernels
      with type instabilities that trigger allocations, so `unrolled_applyat` is
      *required* in order to parallelize over nonuniform iterators on GPUs.

- For benchmarks that indicate performance improvements when using unrolled
  functions with nonuniform iterators, see [Isolated Unrolled Functions](@ref)
  and [Nested Unrolled Functions](@ref)

### Reduction operations with non-constant return types

- `reduce` and `accumulate` have unstable return types when the return type of
  `op` is not constant, but only for iterator lengths greater than 32:

  ```@repl inference_test
  Test.@inferred reduce(tuple, Tuple(1:32));
  Test.@inferred reduce(tuple, Tuple(1:33));
  Test.@inferred unrolled_reduce(tuple, Tuple(1:33));
  ```

- For benchmarks that indicate performance improvements when using unrolled
  functions with nonuniform reductions, see [Isolated Unrolled Functions](@ref)

### Operations with more than 2 levels of recursion

- All functions in Julia have a default "recursion limit" of 2; unless this
  limit is modified, it forces any function that recursively calls itself 2 or
  more times to have an unstable return type:

  ```@repl inference_test
  recursive_length(itr) =
      eltype(itr) <: Tuple ? mapreduce(recursive_length, +, itr) : length(itr)
  Test.@inferred recursive_length(((1, 2), (1, 2, 3)));
  Test.@inferred recursive_length((((1,), (2,)), (1, 2, 3)));
  unrolled_recursive_length(itr) =
      eltype(itr) <: Tuple ?
      unrolled_mapreduce(unrolled_recursive_length, +, itr) : length(itr)
  Test.@inferred unrolled_recursive_length((((1,), (2,)), (1, 2, 3)));
  ```

  !!! note "Note"
      ##### *Is there any other way to avoid the default recursion limit?*

      The default recursion limit applies to all functions defined in `Base` and
      `Base.Iterators`, so those functions will have unstable return types for
      more than 2 levels of recursion, even when all user-defined functions
      passed to them have had their recursion limits disabled. It is also
      impossible to modify the recursion limits of functions defined in `Base`
      from external packages. This means that the only way to avoid the default
      recursion limit is to not use certain functions from `Base`, and instead
      to define alternatives without any recursion limits.

- For benchmarks that indicate performance improvements when using unrolled
  functions with recursive operations, see [Recursive Unrolled Functions](@ref)

## When to Use `StaticOneTo` and `StaticBitVector`

### Iterators of `Int`s from 1 to `N`

```@docs
StaticOneTo
```

If an iterator only contains the integers from 1 to `N ≥ 0`, it is possible to
provide the compiler with the values in the iterator in addition to their types
by using a `StaticOneTo`, as opposed to a `Tuple` or something similar. This
can allow the compiler to fully optimize out code that depends on those values,
essentially moving the code's execution from run time to compilation time:

```@repl inference_test
@code_llvm debuginfo=:none mapreduce(abs2, +, (1, 2, 3))
@code_llvm debuginfo=:none mapreduce(abs2, +, StaticOneTo(3))
```

Standard library functions can sometimes take advantage of this optimization,
but for most non-trivial operations it is necessary to use unrolled functions:

```@repl inference_test
@code_llvm debuginfo=:none mapreduce(log, +, StaticOneTo(3))
@code_llvm debuginfo=:none unrolled_mapreduce(log, +, StaticOneTo(3))
```

For benchmarks that indicate performance improvements when using `StaticOneTo`s,
see [Very Long Iterators](@ref).

!!! note "Note"
    ##### *Can the compiler infer iterator values in other scenarios?*

    The compiler can usually infer the values of iterators that only contain
    [singletons](https://docs.julialang.org/en/v1/manual/types/#man-singleton-types)
    when they are accessed using `Core.Const` indices, but this is not possible
    for non-singletons (e.g., integers) unless some special type of iterator is
    used (e.g., a `StaticOneTo`).

### Long iterators of `Bool`s that get modified across loop iterations

```@docs
StaticBitVector
```

Loops in Julia often allocate memory when a value larger than 32 bytes in size
is modified across loop iterations (regardless of whether the loops are unrolled
or not). Since `Bool`s are represented by bytes, this limits certain types of
loops to modifying [bitmasks](https://en.wikipedia.org/wiki/Mask_(computing)) of
no more than 32 `Bool`s in order to avoid allocations. Unlike an iterator of
`Bool`s, though, a `StaticBitVector` stores 8 bits in every byte, which makes it
possible to modify up to 256 bits at a time in loops without any allocations:

```@repl inference_test
random_bit_flips(itr) = reduce(
    (itr′, i) -> Base.setindex(itr′, !itr′[rand(1:i)], i),
    1:length(itr);
    init = itr,
)
@allocated random_bit_flips(ntuple(Returns(true), Val(32))) # hide
@allocated random_bit_flips(ntuple(Returns(true), Val(32)))
@allocated random_bit_flips(ntuple(Returns(true), Val(33))) # hide
@allocated random_bit_flips(ntuple(Returns(true), Val(33)))
@allocated random_bit_flips(StaticBitVector{256}(true)) # hide
@allocated random_bit_flips(StaticBitVector{256}(true))
```

As with `StaticOneTo`s, standard library functions can occasionally optimize
`StaticBitVector`s as well as unrolled functions, but most complex use cases
require unrolled functions.

For benchmarks that indicate performance improvements when using long
`StaticBitVector`s that get modified across loop iterations, see
[Nested Unrolled Closures](@ref).
