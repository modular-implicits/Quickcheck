Library based off the Haskell QuickCheck library heaving using Implicits.

The user could write something like:


`test_commutativity (x : int) (y : int) = x + y = y + x`

`let () = quickCheck test_commutativity`

And it will respond with either:
  `Success: {num_tests: 1000}`
Or
  `Failure: {num_tests: 0, used_seed: 650494855, failing_case: {ok: false, expect: true, reason: "Falsifiable", test_case: "(936052328) (129383761)", the_exception: None}}`

Quickcheck will also catch exceptions and report them with the relevant test cases:
 ` Failure: {num_tests: 0, used_seed: 1071023854, failing_case: {ok: false, expect: true, reason: "Falsifiable", test_case: "(MkBasic 811252409) (256990194)", the_exception: None}}`
