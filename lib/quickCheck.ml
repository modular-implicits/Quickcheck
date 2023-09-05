module List' = List 
module String' = String
open Imp.Control;;
open Imp.Show;;


(* Can remove int as OCaml handles randomness differently *)
type 'a gen = unit -> 'a

implicit module GenMonad : sig 
  include Functor with type 'b t = 'b gen
  include Applicative with type 'b t := 'b t 
  include Monad with type 'b t := 'b t
end = struct 
  type 'a t = 'a gen
  let return (x : 'a) : 'a t = fun () -> x
  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t = fun () -> f (x ()) ()
  let fmap (f : 'a -> 'b) (x : 'a t) : 'b t = fun () -> f (x ())
  let apply (f : ('a -> 'b) t) (x : 'a t) : 'b t = fun () -> (f ()) (x ())

end

module type Arbitrary = sig
  type t
  val arbitrary : t gen
end

let get_val {A : Arbitrary} () = A.arbitrary ()

implicit module IntArbitrary : Arbitrary with type t = int = struct
  type t = int
  let arbitrary () = Random.int 1000
end

implicit module ListArbitrary {A : Arbitrary} : Arbitrary with type t = A.t list = struct
  type t = A.t list
  let arbitrary () = 
    let rec myLoop (i : int) (acc : A.t list) = if i = 0 then acc else myLoop (i - 1) ((A.arbitrary ()) :: acc) in 
    myLoop (Random.int 1000) []
end

implicit module CharArbitrary : Arbitrary with type t = char = struct
  type t = char
  let arbitrary () = (Char.chr (97 + (Random.int 26)));;
end

implicit module StringArbitrary : Arbitrary with type t = string = struct 
  type t = string 
  let arbitrary () = 
    let rec myLoop (i : int) (acc : string) = if i = 0 then acc else myLoop (i - 1) ((Char.escaped (CharArbitrary.arbitrary ())) ^ acc) in 
    myLoop (Random.int 1000) ""
end

implicit module BoolArbitrary : Arbitrary with type t = bool = struct 
  type t = bool
  let arbitrary () = 
    let rand = Random.int 2 in 
    if rand = 0 then true else false
end

implicit module OptionArbitrary {A : Arbitrary} : Arbitrary with type t = A.t option = struct 
  type t = A.t option
  let arbitrary () = 
    let rand = Random.int 10 in 
    if rand = 0 then None else Some (A.arbitrary ())
end

implicit module UnitArbitrary : Arbitrary with type t = unit = struct 
  type t = unit
  let arbitrary () = ()
end

implicit module PairArbitrary {A : Arbitrary} {B : Arbitrary} : Arbitrary with type t = A.t * B.t = struct 
  type t = A.t * B.t
  let arbitrary () = (A.arbitrary (), B.arbitrary ())
end

open Generics.Generic;;

module type Arbibable = sig 
  type t
  val arbibable : t gen
end

let arbibable {A : Arbibable} = A.arbibable

implicit module ArbibableGenBasic {X : Arbitrary} : Arbibable with type t = X.t genBasic = struct 
  type t = X.t genBasic
  let arbibable () = GenBasic ("", X.arbitrary ())
end

implicit module ArbibableGenProd {X : Arbibable} {Y : Arbibable} : Arbibable with type t = (X.t, Y.t) genProd = struct 
  type t = (X.t, Y.t) genProd
  let arbibable () = GenProd (X.arbibable (), Y.arbibable ())
end

implicit module ArbibableGenSum {X : Arbibable} {Y : Arbibable} : Arbibable with type t = (X.t, Y.t) genSum = struct 
  type t = (X.t, Y.t) genSum
  let arbibable () = let rand = Random.int 2 in 
    if rand = 0 then Left (X.arbibable ()) else Right (Y.arbibable ())
end

implicit module ArbibableGeneric {X : Generic} {XRep : Arbibable with type t = X.rep} : Arbitrary with type t = X.t = struct 
  type t = X.t
  let arbitrary () = X.fromRep (XRep.arbibable ())
end
      


type result = {
  ok                : bool option; (* result of the test case; None = discard *)
  expect            : bool;        (* indicates what the expected result of the property is *)
  reason            : string;     
  test_case         : string;
  the_exception     : exn option;} (* a message indicating what went wrong 
  the_exception     : an_exception option; (* the exception thrown, if any *)
  abort             : bool;        (* if True, the test should not be repeated *)
  maybe_num_tests   : int option;  (* stop after this many tests *)
  maybe_check_coverage : confidence option; (* required coverage confidence *)
  labels            : string list; (* test case labels *)
  classes           : string list; (* test case classes *)
  tables            : (string * string) list; (* test case tables *)
  required_coverage : (string option * string * float) list; (* required coverage *)
  callbacks         : callback list; (* the callbacks for this test case *)
  test_case         : string list; (* the generated test case *) 
} *)

let string_of_exception e = Printexc.to_string e


let basic_result : result = {ok = None; expect = true; reason = ""; test_case = ""; the_exception = None}

let string_of_result r =
  let string_of_ok = function
    | Some true -> "true"
    | Some false -> "false"
    | None -> "discard"
  in
  let string_of_exn = function
    | Some e -> string_of_exception e
    | None -> "None"
  in
  Printf.sprintf "ok: %s, expect: %b, reason: \"%s\", test_case: \"%s\", the_exception: %s"
    (string_of_ok r.ok) r.expect r.reason r.test_case (string_of_exn r.the_exception)

let succeeded : result = {basic_result with ok = Some true} (* ; expect = true}*)
let failed : result = {basic_result with ok = Some false}
let rejected : result = {basic_result with ok = None}

type property = MkProperty of result gen

module type Testable = sig 
  type t 
  val property : t -> property
end

let property' {T : Testable} = T.property

implicit module TestableResult : Testable with type t = result = struct 
  type t = result
  (* taking in a result and want to produce a property *)
  let property (x : result) : property = MkProperty (return x)
end

implicit module TestableUnit : Testable with type t = unit = struct 
  type t = unit
  let liftUnit () = succeeded
  let property (x : unit) : property = property' (liftUnit x)
end

implicit module TestableBool : Testable with type t = bool = struct
  type t = bool
  let liftBool : bool -> result = function 
      | true -> succeeded
      | false -> {failed with reason = "Falsifiable"}
  let property = fun x -> property' (liftBool x)
end

let add_case {S : Show} (test_val : S.t) (r : result) = let space = if r.test_case = "" then "" else " " in 
                          {r with test_case = "(" ^ (S.show test_val) ^ ")" ^ space ^ (r.test_case)}



implicit module TestableFunc {A : Arbitrary} {S : Show with type t = A.t} {T : Testable} : Testable with type t = A.t -> T.t = struct 
  type t = A.t -> T.t
  let property (f : A.t -> T.t) : property = MkProperty (let rand = A.arbitrary in 
                                                            fun () -> let test_val = rand () in 
                                                                      try
                                                                          match T.property (f test_val) with 
                                                                        | MkProperty g -> add_case {S} test_val (g ())
                                                                      with 
                                                                        | e -> add_case {S} test_val {failed with the_exception = Some e})
end

let rand_chr () = (Char.chr (97 + (Random.int 26)));; 

let giveResults {T : Testable} (x : T.t) = 
  let () = Random.self_init ()(* Sets the seed *) in 
  let (MkProperty f) = T.property x in
  for i = 0 to 4 do 
    let result = f () in 
    let () = print_endline (string_of_result result) in 
    ()
  done;

type success_data = {
  num_tests : int;

}

type fail_data = {
  num_tests : int;
  used_seed : int;
  failing_case : result;
}

type testResult = Success of success_data | Failure of fail_data

let string_of_testResult tr =
  match tr with
  | Success s ->
      Printf.sprintf "Success: {num_tests: %d}" s.num_tests
  | Failure f ->
      Printf.sprintf "Failure: {num_tests: %d, used_seed: %d, failing_case: {%s}}"
        f.num_tests f.used_seed (string_of_result f.failing_case)


let quickCheck {T : Testable} (x : T.t) = 
  let seed = Random.bits () in
  let () = Random.init seed in (* Sets the seed to be random *)

  let rec myLoop (i : int) (f : unit -> result) = if i = 1000 then Success {num_tests = i} else 
    let result = f () in 
      match result.ok with
      | Some true -> myLoop (i + 1) f
      | Some false -> Failure {num_tests = i; used_seed = seed; failing_case = result}
  in 

  let (MkProperty f) = T.property x in
      let my_result = myLoop 0 f in
      print_endline (string_of_testResult my_result)
      


      