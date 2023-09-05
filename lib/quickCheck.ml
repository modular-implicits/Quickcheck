(* Test.QuickCheck *)

module List' = List;;
open Imp.Control;;


(* Want to be able to do 
   prop_reverse :: [Int] -> Bool
   prop_reverse xs = reverse (reverse xs) == xs*)



type 'a gen = int -> unit -> 'a

implicit module GenMonad : sig 
  include Functor with type 'b t = 'b gen
  include Applicative with type 'b t := 'b t 
  include Monad with type 'b t := 'b t
end = struct 
  type 'a t = 'a gen
  let fmap (f : 'a -> 'b) (x : 'a gen) : 'b gen = fun n -> fun s -> f (x n s)
  let return (x : 'a) : 'a gen = fun _ _ -> x
  let apply (f : ('a -> 'b) gen) (x : 'a gen) : 'b gen = fun n -> fun s -> (f n s) (x n s)
  let bind (x : 'a gen) (f : 'a -> 'b gen) : 'b gen = fun n -> fun s -> f (x n s) n s
end

type result = {
  ok                : bool option; (* result of the test case; None = discard *)
  expect            : bool;        (* indicates what the expected result of the property is *)
  reason            : string;     } (* a message indicating what went wrong 
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

exception Not_Implemented


let basic_result : result = {ok = raise Not_Implemented; expect = true; reason = ""}

let succeeded : result = {basic_result with ok = Some true} (* ; expect = true}*)
let failed : result = {basic_result with ok = Some false}
let rejected : result = {basic_result with ok = None}

type 'a rose = MkRose of 'a * ('a rose list)

implicit module RoseMonad : sig 
  include Functor with type 'b t = 'b rose
  include Applicative with type 'b t := 'b t
end = struct 
  type 'b t = 'b rose
  let rec fmap (f : 'a -> 'b) (x : 'a rose) : 'b rose = match x with 
    | MkRose (a, rs) -> MkRose (f a, List'.map (fmap f) rs)
  let return (x : 'a) : 'a rose = MkRose (x, [])
  let apply (f : ('a -> 'b) rose) (x : 'a rose) : 'b rose = match f with 
    | MkRose (f, rs) -> fmap f x
end 


type prop = MkProp of result rose

type property = MkProperty of prop gen

(* Something that protects rose from exceptions *)

module type Arbitrary = sig
  type t
  val arbitrary : t gen
end

module type Testable = sig 
  type t 
  val property : t -> property
end

let property' {T : Testable} = T.property

let liftBool : bool -> result = function 
  | true -> succeeded
  | false -> {failed with reason = "Falsifiable"}

  
implicit module TestableResult : Testable with type t = result = struct 
  type t = result
  (* taking in a result and want to produce a property *)
  let property (x : result) : property = MkProperty (return (MkProp (return x)))
end



implicit module TestableBool : Testable with type t = bool = struct
  type t = bool
  let property = fun x -> property' (liftBool x)
end



(*


(* quickCheck {T : Testable} (prop : T.t) : unit *)

(* Running example with integers so prop : int -> bool *)

implicit module IntArbitrary : Arbitrary with type t = int = struct
  type t = int
  let arbitrary n () = Random.int n
end



*)