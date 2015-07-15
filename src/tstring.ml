type tstring = (* to avoid recopy better than string list *)
  | Node of tstring list
  | Str of string

let  tstring_to_string t =
  let rec to_ a t =
    match t with 
      | Node list -> List.fold_left to_ a list
      | Str s -> a ^ s
  in
  to_ "" t

let print t =
  let rec to_ t =
    match t with 
      | Node list -> List.iter to_ list
      | Str s -> print_string s
  in
  to_ t
 
let test() =
  print ( Node [ Str "plop" ; Str " yo"; Node [Str "\n"; Node[]] ] )
