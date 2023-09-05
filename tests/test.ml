module List' = List;;

open Testing.QuickCheck;;
open Imp.Show;;



let checkReflexitivity (x : int) = x = x

let commutativity (x : int) (y : int) = x + 9 = y + x

let reversing (xs : int list) = List'.rev (List'.rev xs) = xs


let () = 
print_endline "Testing reflexitivity";
quickCheck checkReflexitivity;;

let () =
  print_endline "Testing commutativity";
  quickCheck commutativity;;

let () =
  print_endline "Testing reversableness";
  quickCheck reversing;;