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