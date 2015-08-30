open Tstringop

let tstring_to_string t =
  let rec to_ a t =
    match t with 
      | Tstr list -> List.fold_left to_ a list
      | Str s -> a ^ s
  in
  to_ "" t

let print t =
  let rec to_ t =
    match t with 
      | Tstr list -> List.iter to_ list
      | Str s -> print_string s
  in
  to_ t
 
let test() =
  let s = ( Tstr [ Str "plop" ; Str " yo"; Tstr [Str "\n"; Tstr[]] ] ) in
  print s;
  s |> tstring_to_string |> print_string 


(*let _ = test ()*)
