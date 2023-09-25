module List' = List;;

open Testing.QuickCheck;;
(* open Imp.Show;; *)


exception Not_Implemented


let checkReflexitivity (x : int) = x = x

let commutativity (x : int) (y : int) = x + 9 = y + x

let reversing (xs : int list) = List'.rev (List'.rev xs) = xs

let testExceptUnit () : unit = raise Not_Implemented

let () = 
  let open [@warning "-33"] Imp.Show in
print_endline "Testing reflexitivity";
quickCheck checkReflexitivity;;

let () =
  let open [@warning "-33"] Imp.Show in
  print_endline "Testing commutativity";
  quickCheck commutativity;;

let () =
  let open [@warning "-33"] Imp.Show in
  print_endline "Testing reversableness";
  quickCheck reversing;;

let () =
  let open [@warning "-33"] Imp.Show in
  print_endline "Test unit func";
  quickCheck testExceptUnit;;

(* Generics example not working sadly *)

open Generics.Generic;;

type evenBasic = MkBasic of int

implicit module GenEvenBasic : Generic with type t = evenBasic and type rep = int genBasic = struct 
  type t = evenBasic
  type rep = int genBasic
  let toRep = function MkBasic x -> GenBasic ("MkBasic", x)
  let fromRep = function GenBasic (_, x) -> MkBasic x
end

let lol (_ : evenBasic) (_ : int) : bool = false


let () = 
  let open [@warning "-33"] Imp.Show in
  let open [@warning "-33"] Generics.Generic in
  let open [@warning "-33"] Generics.GenShow in
  print_endline "Testing generics";
  quickCheck lol;;