Library based off the Haskell QuickCheck making heavy use of Implicits.

The user could write something like:


```
test_commutativity (x : int) (y : int) = x + y = y + x
let () = quickCheck test_commutativity
```

And it will respond with either:
  `Success: {num_tests: 1000}`
Or
  `Failure: {num_tests: 0, used_seed: 650494855, failing_case: {ok: false, expect: true, reason: "Falsifiable", test_case: "(936052328) (129383761)", the_exception: None}}`

Quickcheck will also catch exceptions and report them with the relevant test cases:
 ` Failure: {num_tests: 0, used_seed: 275906564, failing_case: {ok: false, expect: true, reason: "", test_case: "(())", the_exception: Dune__exe__Test.Not_Implemented}}`

For custom types, that do not have an `Arbitrary` instance, it is easiest to provide the `Generics` implementation of the type, as that provides a default `Arbitrary` implementation.
