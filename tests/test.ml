module List' = List;;

open Testing.QuickCheck;;
open Imp.Show;;


exception Not_Implemented


let checkReflexitivity (x : int) = x = x

let commutativity (x : int) (y : int) = x + 9 = y + x

let reversing (xs : int list) = List'.rev (List'.rev xs) = xs

let testExceptUnit () : unit = raise Not_Implemented

let () = 
print_endline "Testing reflexitivity";
quickCheck checkReflexitivity;;

let () =
  print_endline "Testing commutativity";
  quickCheck commutativity;;

let () =
  print_endline "Testing reversableness";
  quickCheck reversing;;

let () =
  print_endline "Test unit func";
  quickCheck testExceptUnit;;

(* Generics example not working sadly *)

open Generics.Generic;;
open Generics.GenShow;;

type evenBasic = MkBasic of int

implicit module GenEvenBasic : Generic with type t = evenBasic and type rep = int genBasic = struct 
  type t = evenBasic
  type rep = int genBasic
  let toRep = function MkBasic x -> GenBasic ("MkBasic", x)
  let fromRep = function GenBasic (_, x) -> MkBasic x
end

module XRep : Arbibable with type t = int genBasic = ArbibableGenBasic{IntArbitrary}
module ArbibableGenEvenBasic : Arbitrary with type t = evenBasic = ArbibableGeneric{GenEvenBasic}{XRep}

let lol (x : evenBasic) : bool = false

let z : evenBasic = get_val {ArbibableGenEvenBasic} ()


let () = 
  print_endline "Testing generics";
  quickCheck lol;;