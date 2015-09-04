type t  = 
    (*Literals*)
  | LInt of int
  | LString of string 
  | LFloat of float 
	(*Internal type*)
  | Integer 
  | Float 
  | String 
      (*Op*)
  | And of t * t
  | Or  of t * t
  | Xor of t list
	(* Nommé *)
  | Name of string 
	(* Pointeur avec un path *)
  | Ref of string list (* TODO: adding integer index ref *)
	(* Multiplicité *)
  | Mult of int * int
(* | Not of t TODO: to complete *)

let sep = " " (*this is to avoid many " " string*)
(*
 a & b
^ c
^ d 
  | e
  | f & g
*)

open Tstringop

let rec to_string t =
  let rec to_string_ (st:string) front prev t =
    let separation =
      match t, prev with 
      | Xor _, Xor _ 
      | Or _, Or _
      | And _, And _ -> false
      | _, _ -> true
    in
    let st =
      match separation,t with
        | _, And _ -> st
        | true, _ -> String.concat "" [st;" "] 
        | false, _ -> st
    in
    ( match t , separation  with
    | LInt i , _ -> [string_of_int i ]
    | LString s,_  -> ["\"";s;"\""] 
    | LFloat f ,_-> [string_of_float f] 
    | Integer ,_-> ["Integer"]
    | Float ,_-> ["Float"]
    | String ,_-> ["String"]
    | Name n ,_  -> [n]
    | Ref l ,_-> ["$"; String.concat "/" l ] 
    | Mult (min,max) ,_-> ["{"; string_of_int min; sep; string_of_int max; "}"] 
    | And (a, b), true ->
	let symb = 
	  match a with 
	  | Name _ -> " = "
	  | _ ->  " & " 
	in
	List.flatten [ ["\n"]; to_string_ st "" t a ;
                       symb :: to_string_ st "" t b;]
    | And (a, b), false ->
	let symb = 
	  match a with 
	  | Name _ -> " = "
	  | _ ->  " & " 
	in
	List.flatten [ to_string_ st "" t a ; symb :: to_string_ st "" t b;]
    | Or (a, b) , true -> 
	let s = ["\n"; st ; "| "] in
	let x = List.flatten 
	    [ s; to_string_ st t a ; s; to_string_ st t b; ]
	in
        x 
    | Or (a, b) , false -> 
	let s = ["\n"; st ; "| "] in
	let x = List.flatten 
	  [ to_string_ st t a ; s; to_string_ st t b; ]
	in
        x 
    | Xor l , true -> 
	let x = "{ ":: List.flatten (
	List.map (fun s -> "\n" :: st :: to_string_ st t s)  l ) @ [" }"]
	in
        x 
    | Xor (h::l) , false ->
	let x = (to_string_ st t h )
	@   
	List.flatten (
	List.map (fun s -> "\n" :: st :: to_string_ st t s)  l )
	in
        x 
    | _ , _ -> []
     )  
  in
  to_string_ "" (Name "root") t 

(*
let rec to_string t =
  let rec to_string_ (st:string) prev t =
    let separation =
      match t, prev with 
      | Xor _, Xor _ 
      | Or _, Or _
      | And _, And _ -> false
      | _, _ -> true
    in
    let st =
      match separation,t with
        | _, And _ -> st
        | true, _ -> String.concat "" [st;" "] 
        | false, _ -> st
    in
    ( match t , separation  with
    | LInt i , _ -> [string_of_int i ]
    | LString s,_  -> ["\"";s;"\""] 
    | LFloat f ,_-> [string_of_float f] 
    | Integer ,_-> ["Integer"]
    | Float ,_-> ["Float"]
    | String ,_-> ["String"]
    | Name n ,_  -> [n]
    | Ref l ,_-> ["$"; String.concat "/" l ] 
    | Mult (min,max) ,_-> ["{"; string_of_int min; sep; string_of_int max; "}"] 
    | And (a, b),_->
	let symb = 
	  match a with 
	  | Name _ -> " = "
	  | _ ->  " & " 
	in
	List.flatten [ to_string_ st t a ; symb :: to_string_ st t b;]
    | Or (a, b) , true -> 
	let s = ["\n"; st ; "| "] in
	let x = List.flatten 
	    [ s; to_string_ st t a ; s; to_string_ st t b; ]
	in
        x 
    | Or (a, b) , false -> 
	let s = ["\n"; st ; "| "] in
	let x = List.flatten 
	  [ to_string_ st t a ; s; to_string_ st t b; ]
	in
        x 
    | Xor l , true -> 
	let x = "{ ":: List.flatten (
	List.map (fun s -> "\n" :: st :: to_string_ st t s)  l ) @ [" }"]
	in
        x 
    | Xor (h::l) , false ->
	let x = (to_string_ st t h )
	@   
	List.flatten (
	List.map (fun s -> "\n" :: st :: to_string_ st t s)  l )
	in
        x 
    | _ , _ -> []
     )  
  in
  to_string_ "" (Name "root") t 
  *)
 
(********************************)

let (|>) a b =
  b a

let print t =
  t |> to_string |> String.concat ""  |> print_string 

let print_bool b =
  if b then 
    print_string "ok" 
  else
    print_string "false" 
      
let or_some a b =
  match a with
  | Some _ -> a
  | None -> b


(** Find a path inside a tree 
  * root = (Name a & (  Name b & plop  xor _ )  ) Xor (Name _ & ( Name _ xor _))
  * Ref ["a";"b"] return (Name b & plop)
  *)
let rec find path root =
  path |> String.concat "/" |> print_string;
  print_endline "";
  print root;
  print_endline "";
  let rec isNameFind current name =
    match current with   
    | And (c, d)
    | Or (c, d) -> isNameFind c name || isNameFind d name 
    | Xor _  -> false (* sous structure *) 
    | Name n -> n = name   
    | _ -> false
  in
  let rec dig name root =
    match root with
    | And _
    | Or _ -> (
	if isNameFind root name then
	  Some root
	else
	  None
       )
    | Xor l ->
	(List.fold_left ( fun a b -> or_some a (dig name b)) None l)
    | _ -> None
  in
  let rec digXor name root =
    match root with
    | And (a,b)
    | Or (a,b) -> or_some (digXor name a ) (digXor name b)
    | Xor l -> dig name root
    | _ -> None
  in
  match path with
  | [] -> Some root
  | name::l -> (
      match digXor name root with
      | Some r -> find l r
      | None -> None
     )
(**
   * Attention au cas Ref qui nécessite une racine pour trouver les réferences
*)
	
(*c'est commutatif, cela permet de réduire les cas*)
let rec andCheckOne a b root =
  (*print_string "((";
  print a; print_string "==";print b ;
  print_string ")";*)
  let r =
    match a,b with
    | Integer, Integer -> true
    | Float, Float -> true
    | String, String -> true
    | Integer, LInt _ -> true 
    | Float, LFloat _ -> true
    | String, LString _ -> true
    | LString s1, LString s2 -> s1 == s2 (*tbc*)
    | LFloat f1, LFloat f2 -> f1 == f2
    | LInt i1, LInt i2 -> i1 == i2 
    | a, And (b,c) -> (andCheck a b root) && (andCheck a c root)
    | a, Or (b,c) -> (andCheck a b root) || (andCheck a c root)
    | Xor [] , Xor [] -> true  
    | Xor (a::c) , Xor (b::d) -> andCheck a b root && andCheck (Xor c) (Xor d) root
    | Name _ , _ -> true
    | Ref path , b ->
	let ref = find path root in (*todo: same name at different place must be catch*)
	( match ref with
	| Some node -> andCheck node b root
	| None -> false (*? unbound reference are always a bug ?*)
	 )
    | Mult (min, max), Mult (min2, max2) -> min <= min2 && max2 <= max (* l'un est inclus dans l'autre *)
    | Mult (min,max), Xor l -> (**xor : array, composition*)
	let rec check_array min max a =
	  (*print_string "check_array "; print_int min ; 
	    print_string " "; print_int max; print_endline "";*)
	  match a with (*! lent : o(n)*) 
	  | [] -> min <= 0 && 0 <= max
	  | (Xor b) :: [] -> check_array min max b 
	  | (Xor b) :: l  -> check_array min max (b@l) 
	  | Or (b,c) :: l -> (check_array min max (b::l)) || (check_array min max (c::l))
 	  | And (b,c) :: l ->  (check_array min max (b::l)) && (check_array min max (c::l))
	  | a :: l -> check_array (min-1) (max-1) l
	in
	check_array min max l
    | _ -> false
  in
  (*print_bool r; print_string ")";*)
  r
and andCheck a b root =
  (andCheckOne a b root ) || (andCheckOne b a root )
    

let rec check root =
  check_with_root root root
and check_with_root node root = 
  match node with 
  | And (a,b) -> andCheck a b root
  | Or (a,b) -> check_with_root a root && check_with_root b root
  | Xor l -> List.fold_left (fun a t -> a && check_with_root t root) true l
  | _ -> true
	
let rec travel f node =
  match node with
  | And (a,b) 
  | Or (a,b) -> travel f a ; travel f b;  
  | Xor l  -> List.iter (travel f) l 
  | _ -> f node   

(** print only once definition of Ref of the node in the context of root
 *)
let print_dependancies node root =
  let res = ref [] in
  let print_def node =
    match node with
    | Ref r ->  
	let def = find r root in
	( match def with 
	| Some n ->
	    (if (not (List.exists (fun o -> o = n) (!res))) then
	      res := (n :: !res)
	    )
	| None -> ()
	 )
    | _ -> () 
  in
  travel print_def node;
  List.iter (fun n -> print n; print_endline "";) (!res)
    
let rec check_to_debug1 root =
  check_to_debug_ root root
and check_to_debug_ node root err =
  let print_ node root =
    print_string "Type error : ";
    print node; print_string "\n";
    print_dependancies node root;   
  in
  (*print_string ":"; print node;print_endline ""; 
    print_string "  :"; print root; print_endline ""; *)
(*  ( if ( not ( check_with_root node root ) ) then (
    print_ node root 
    ) 
    ) ; *)
  let err_ = if(not ( check_with_root node root )) then
    ( Some node )
  else
    None
  in
  (
   match node with 
   | Or (b,c) 
   | And (b,c) -> check_to_debug_ b root err_; check_to_debug_ c root err_
   | Xor l -> List.iter (fun a -> check_to_debug_ a root err_) l  
   | _ -> print_ node root
  )
(** Find the minimum error 
    print reference definition involve
 *)
let rec check_to_debug root =
  check_to_debug_ root root None
and check_to_debug_ node root err =
  let print_ node root =
    print_string "Type error : ";
    print node; print_string "\n";
    print_dependancies node root;   
  in
  if ( not ( check_with_root node root ) ) then
    (
     match node with 
     | Or (b,c) 
     | And (b,c) -> check_to_debug_ b root (Some node); check_to_debug_ c root (Some node) 
     | Xor l -> List.iter (fun a -> check_to_debug_ a root (Some node)) l  
     | _ -> ()
    ) else (
    match err with 
    | Some n -> print_ n root
    | None -> ()
  )

let rec simplify node =
  let rec simplify_xor a list =
    List.rev (List.fold_left dig_ a list)
  and dig_ a xor =
    match xor with
    | Xor l -> simplify_xor a l
    |  h -> (simplify h)::a 
  in
  match node with 
  | Xor (h::[]) -> simplify h
  | Xor l -> Xor (simplify_xor [] l)
(*  | Xor ((Xor l) :: t) -> Xor ( (List.map simplify l) @ (List.map simplify t) )  
    | Xor (h :: (Xor l) :: t) -> Xor ( (simplify h)::(List.map simplify l) @ (List.map simplify t) )  
    | Xor l -> Xor ( List.map simplify l )*)
  | Or (a,b) -> Or ((simplify a), (simplify b))
  | And (a,b) ->  And ((simplify a), (simplify b))
  | a -> a



(*let check a =
  match a with 
  | And (Or (a,b), c)-> true (* (same_type a c) || same_type b c *)
  | Or (e, f)-> true (*(same_type e f) *)(*|| same_type b c*) 
	(*| And (c, (Or (a,b))) -> same_type a c || same_type b c *)
  | _ -> true    
*)

(*option *)
let (&) a b = And (a, b)
let (||) a b = Or (a, b) 
let (^) a b = Xor [a;b] 

(*
let a = And (LInt 1, Integer)
let aa = (LInt 1) & Integer
let b = And (LFloat 1.0, Float)
let bb = (LFloat 1.0) & Float
let dd = Integer || Float
let c = Or (a ,Integer)
let cc = ( a || Integer ) & Float*)
(*let d = (b ^ a) & (b ^ a)*)
(*let e = (b ^ a) & (a ^ b)*)

(*let f = Space f1*)
(*let b = And (Lit 1, Float) (*false*)

let g = LInt 1 &  ( LString "toto" || Integer)
let gg = (g ^ g ^ g) & Mult ( 0, 2)
*)


(*let _ = check a*)

(*

a = b ((c^d^(e||h)^(i&j))^g & f) 

*)

let tt a =
  if check a then 
    print_string "ok\n" 
else
    print_string "false\n" 

let t a =
  print a; 
  print_string " ";
  tt a;
  check_to_debug a

let _ = 
 (**array check*)
 (* print a; tt a;
  print b; tt b;
  print g; tt g;
  print c; tt c;
  print cc; tt cc;
  print gg; tt gg*)
  (*let g = Integer in
  let ggggg = (g ^ g ^ g ^ g) & Mult ( 2, 3) in
  let gggg = (g ^ g ^ g) & Mult ( 2, 3) in
  let ggg = (g ^ g) & Mult ( 2, 3) in
  let gg = (g ) & Mult ( 2, 3) in
  
  
  t gg ;
  t ggg; 
  t gggg ;
  t ggggg ;
*)(*
  let r = (Name "toto" & Integer)
  ^ ( Name "error" & Ref ["toto"] & LFloat 2. & Integer ) 
  ^ ( Name "r" & Ref ["toto"] & LInt 2  ) 
  ^ (Name "struct" & ( (Name "a" & Ref ["toto"]) ^ (Name "aa" & Ref ["toto"] & LFloat 2.0))) 
  ^ (Name "b" & Ref ["toto"])
  ^ (Name "st" & Ref ["struct"] & ((Ref ["struct";"a"] & ( Integer ^ Float)) ^ (Ref ["struct";"aa"] & LInt 2)))
  ^ (Name "enum_t" & (Name "token1" || Name "token2"))
  ^ (Name "enum_var" & Ref ["enum_t";"token1"]) 
  ^ ((Ref ["toto"] || Float || Ref ["enum_t"]) & Name "wierd" & (Name "a" ^ Name "b"))*)
  let r = (Name "toto" & Integer)
    ^ (Name "enum_t" & ((Name "token1" & Integer) 
		      || (Float & Name "token2" & (Name "a" ^ Name "b"))) )
    ^ ((Ref ["toto"] || Float || Ref ["enum_t"]) 
       & Name "wierd" & (Name "a" ^ Name "b"))
    ^ (Name "Yo" & Name "Yoyo" & Name "Yi" & (Name "x" ^ Name "y" ^ Name "z"))
    ^ (Name "A" & (Name "a" ^ Name "b"))
    ^ (Name "A" & (Name "a" || Name "b"))
    ^ (Name "A" & (Name "a" & Name "b"))
    ^ (Name "A" || (Name "a" ^ Name "b"))
    ^ (Name "A" || (Name "a" || Name "b"))
    ^ (Name "A" || (Name "a" & Name "b"))
    ^ (Name "A" ^ (Name "a" ^ Name "b"))
    ^ (Name "A" ^ (Name "a" || Name "b"))
    ^ (Name "A" ^ (Name "a" & Name "b"))
    ^ (Name "X" ^ Name "Y" ^ Name "Z" ^ Name "T")
    ^ (Name "X" ^ Name "Y" ^ Name "Z" || Name "T")
    ^ (Name "X" ^ Name "Y" ^ Name "Z" & Name "T")
    ^ (Name "X" ^ Name "Y" || Name "Z" ^ Name "T")
    ^ (Name "X" ^ Name "Y" || Name "Z" || Name "T")
    ^ (Name "X" ^ Name "Y" || Name "Z" & Name "T")
    ^ (Name "X" ^ Name "Y" & Name "Z" ^ Name "T")
    ^ (Name "X" ^ Name "Y" & Name "Z" || Name "T")
    ^ (Name "X" ^ Name "Y" & Name "Z" & Name "T")
    ^ (Name "X2" || Name "Y" ^ Name "Z" ^ Name "T")
    ^ (Name "X" || Name "Y" ^ Name "Z" || Name "T")
    ^ (Name "X" || Name "Y" ^ Name "Z" & Name "T")
    ^ (Name "X" || Name "Y" || Name "Z" ^ Name "T")
    ^ (Name "X" || Name "Y" || Name "Z" || Name "T")
    ^ (Name "X" || Name "Y" || Name "Z" & Name "T")
    ^ (Name "X" || Name "Y" & Name "Z" ^ Name "T")
    ^ (Name "X" || Name "Y" & Name "Z" || Name "T")
    ^ (Name "X" || Name "Y" & Name "Z" & Name "T")
    ^ (Name "X3" & Name "Y" ^ Name "Z" ^ Name "T")
    ^ (Name "X" & Name "Y" ^ Name "Z" || Name "T")
    ^ (Name "X" & Name "Y" ^ Name "Z" & Name "T")
    ^ (Name "X" & Name "Y" || Name "Z" ^ Name "T")
    ^ (Name "X" & Name "Y" || Name "Z" || Name "T")
    ^ (Name "X" & Name "Y" || Name "Z" & Name "T")
    ^ (Name "X" & Name "Y" & Name "Z" ^ Name "T")
    ^ (Name "X" & Name "Y" & Name "Z" || Name "T")
    ^ (Name "X" & Name "Y" & Name "Z" & Name "T")
    ^ (Name "X4" ^ Name "Y" & Name "Z" || Name "T")
    ^ (Name "X" ^ (Name "Y" & (Name "Z" || Name "T")))
    ^ (Name "X5" & Name "Y" ^ Name "Z" || Name "T")
    ^ (Name "X" & (Name "Y" ^ (Name "Z" || Name "T")))
    ^ (Name "X6" & Name "Y" || Name "Z" ^ Name "T")
    ^ (Name "X" & (Name "Y" || (Name "Z" ^ Name "T")))
    ^ (Name "X7" || Name "Y" & Name "Z" ^ Name "T")
    ^ (Name "X" || (Name "Y" & (Name "Z" ^ Name "T")))
    ^ (Name "X8" ^ Name "Y" || Name "Z" & Name "T")
    ^ (Name "X" ^ (Name "Y" || (Name "Z" & Name "T")))
    ^ (Name "X9" || Name "Y" ^ Name "Z" & Name "T")
    ^ (Name "X" || (Name "Y" ^ (Name "Z" & Name "T")))


 in
  print r;print_endline "\n";
  (*print_endline "simplify :";
  print (simplify r);print_endline "\n";*)
  (*print ( r |> simplify |> simplify);print_endline "\n";*)
  (*t r;*)


(*

	    | LInt i -> 
	    | LString s -> 
	    | LFloat f -> 
	    | Integer -> 
	    | Float -> 
	    | String -> 
	    | And (a,b)-> 
	    | Or (a,b) ->  
	    | Xor l  -> 
	    | Name n -> if n = a then	  
	    | Ref _ -> 
	    | Mult _-> 
*)
