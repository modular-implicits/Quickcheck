open Testing.QuickCheck;;
open implicit Imp.Show;;



let checkReflexitivity (x : int) = x = x

let commutativity (x : int) (y : int) = x + 9 = y + x



let () = 
print_endline "Testing reflexitivity";
quickCheck checkReflexitivity;;

let () =
  print_endline "Testing commutativity";
  quickCheck commutativity;;