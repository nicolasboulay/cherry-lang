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
	(* Named *)
  | Name of string 
	(* Pointer with path *)
  | Ref of string list (* TODO: adding integer index ref, + reference  and ../ as directory *)
	(* Multiplicité *)
  | Mult of int * int (*min/max*)
(* | Not of t TODO: to complete *)
(* | Unordered : to check contener without order constraint*)
open Tstringop

let sep = Str " " (*this is to avoid many " " string*)

let to_string t =
  let to_string_simple t =
    match t with
    | LInt i  -> Str (string_of_int i )
    | LString s  -> Tstr [Str "\""; Str s; Str "\""] 
    | LFloat f -> Str (string_of_float f ) 
    | Integer -> Str "Integer"
    | Float ->  Str "Float"
    | String -> Str "String"
    | Name n -> Tstr [Str ""; Str n;Str "="]
    | Ref [] -> Str "/"
    | Ref (h::l) -> Tstr ((Str h) ::
	( List.fold_right (fun (e:string) (a:tstring list)  -> (Str "/") :: (Str e) :: a ) l  []  ))
    | Mult (min,max) -> Tstr [Str "{"; Str (string_of_int min); sep; Str (string_of_int max); Str "}"] 
    | And _ | Or _ | Xor _ -> assert false
  in
  let rec to_string_ st prev t =
    let separation =
      match t, prev with 
      | Xor _, Xor _ 
      | Or _, Or _
      | And _, And _ -> false
      | _, _ -> true
    in
    let st =
      match separation, t with
        (*| _, And _ -> st*)
        | true, _ -> Tstr [st; Str "  "] 
        | false, _ -> st
    in
    match t, separation, prev with 
    | And (a, b), _, _ ->
	Tstr [ to_string_ st t a; sep; to_string_ st t b]
    | Or (a,b), _, Or _ -> 
	Tstr [ to_string_ st t a; 
	       Str "\n";st;Str "| ";to_string_ st t b]
    | Or (a,b), _, _ -> 
	Tstr [ Str "\n";st;Str "| ";to_string_ st t a; 
	       Str "\n";st;Str "| ";to_string_ st t b]
    | Xor (a::[]), _, Xor _ -> Tstr [to_string_ st t a;]  
				     
    | Xor (a::tl), _, Xor _ -> Tstr [to_string_ st t a;  
				     Str "\n";st;Str "^ "; to_string_ st t (Xor tl) ;]
    | Xor (a::[]), _, _ -> Tstr [Str "\n";st; Str "^ "; to_string_ st t a;]  
    | Xor (a::tl), _, _ -> Tstr [Str "\n";st; Str "^ "; to_string_ st t a;  
				 Str "\n";st; Str "^ "; to_string_ st t (Xor tl) ;] 
    | Xor [], _, _ -> Str ""
    | a, _ , _ -> to_string_simple a 
  in
  to_string_ (Str "") (Name "root") t 

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
  (*path |> String.concat "/" |> print_string;*)
(*  print_endline "";
  print root;
  print_endline "";*)
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
type error = 
  | No
  | BasicTypeMismatch of t*t
  | UnknownReference of t
  | IncompatibleMultiplicity of t * t
  | Multiplicity of t * t
  | Other of t * t

let print_error e =
  let p_ a b =
    Tstring.print (to_string a); 
    print_endline "\n---";
    Tstring.print (to_string b); print_endline "" 
  in
  match e with 
  | No -> print_string "No error"
  | UnknownReference a -> print_endline "Unknown reference :"; 
      Tstring.print (to_string a)
  | BasicTypeMismatch (a,b)-> print_endline "Basic type mismatch :"; p_ a b
  | IncompatibleMultiplicity (a,b) -> print_endline "Incompatible multiplicity"; p_ a b
  | Multiplicity (a,b) -> print_endline "Wrong multiplicty"; p_ a b
  | Other (a,b) -> print_string "Error "; p_ a b

(*let ( &&& ) a b = 
  match a, b with 
  | No, No -> No
  | No, _ -> b
  | _,_ -> a 

let ( ||| ) a b = 
  match a, b with 
  | _, No 
  | No, _ -> No
  | _,_ -> a 
*)
let ( ||| ) a b = 
  match a with 
  | No -> No
  | _ -> b ()

let ( &&& ) a b = 
  match a with 
  | No -> b ()
  | _ -> a





(*c'est commutatif, cela permet de réduire les cas*)
let rec andCheckOne a b root =
  let check_ expr error =
    if expr then
      error
    else 
     No
  in(*
  print_string "(";
  Tstring.print (to_string a); print_string " == ";Tstring.print (to_string b) ;
  print_string ")\n";*)
  let r =
    match a,b with
    | Integer, Integer -> No
    | Float, Float -> No
    | String, String -> No
    | Integer, LInt _ -> No
    | Float, LFloat _ -> No
    | String, LString _ -> No
    | LString s1, LString s2 -> check_ (s1 == s2) (BasicTypeMismatch (a,b)) (*tbc*)
    | LFloat f1, LFloat f2 -> check_ (f1 == f2) (BasicTypeMismatch (a,b))
    | LInt i1, LInt i2 -> check_ (i1 == i2)  (BasicTypeMismatch (a,b))
    | a, And (b,c) -> (andCheck a b root) &&& (fun () -> andCheck a c root)
    | a, Or (b,c) -> (andCheck a b root) ||| (fun () -> andCheck a c root)
    | Xor [] , Xor [] -> No  
    | Xor (a::c) , Xor (b::d) -> andCheck a b root &&& (fun() -> andCheck (Xor c) (Xor d) root)
    | Name _ , _ -> No
    | Ref path , b ->
	let ref = find path root in (*todo: same name at different place must be catch*)
	( match ref with
	| Some node -> andCheck node b root
	| None -> UnknownReference a (*? unbound reference are always a bug ?*)
	 )
    | Mult (min, max), Mult (min2, max2) -> (* l'un est inclus dans l'autre *)
	check_ (min <= min2 && max2 <= max) (IncompatibleMultiplicity (a,b))
    | Mult (min,max), Xor l -> (**xor : array, composition*)
	let rec check_array min max ll =
	  (*print_string "check_array "; print_int min ; 
	    print_string " "; print_int max; print_endline "";*)
	  match ll with (*! lent : o(n)*) 
	  | [] -> check_ (min <= 0 && 0 <= max) (Multiplicity (a,b))
	  | (Xor b) :: [] -> check_array min max b 
	  | (Xor b) :: l  -> check_array min max (b@l) 
	  | Or (b,c) :: l -> (check_array min max (b::l)) ||| (fun () -> check_array min max (c::l))
 	  | And (b,c) :: l ->  (check_array min max (b::l)) &&& (fun () -> check_array min max (c::l))
	  | a :: l -> check_array (min-1) (max-1) l
	in
	check_array min max l
    | _ -> Other (a,b)
  in
  (*print_bool r; print_string ")";*)
  r
and andCheck a b root =
  (andCheckOne a b root ) ||| ( fun () -> andCheckOne b a root )

let rec check root =
  check_with_root root root
and check_with_root node root = 
  match node with 
  | And (a,b) -> andCheck a b root
  | Or (a,b) -> check_with_root a root &&& (fun () -> check_with_root b root)
  | Xor l -> List.fold_left (fun a t -> a &&& (fun() -> check_with_root t root)) No l
  | _ -> No

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
  List.iter (fun n -> to_string n |> Tstring.print; print_endline "";) (!res)

(** Find the minimum error 
    print reference definition involve
 *)
let rec check_to_debug root =
  check_to_debug_ root root None
and check_to_debug_ node root err =
  let print_ node root =
    print_string "Type error : ";
    to_string node |> Tstring.print; print_string "\n";
    print_endline " Dependancies : ";
    print_dependancies node root;   
  in
  match check_with_root node root with
  | No -> ()
  | UnknownReference a -> print_ a root
  | BasicTypeMismatch (a,b)
  | IncompatibleMultiplicity (a,b)
  | Multiplicity (a,b)
  | Other (a,b) -> print_ a root; print_ b root	
  (*if ( not ( check_with_root node root ) ) then
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
  )*)

let (&) a b = And (a, b)
let (||) a b = Or (a, b) 
let (^) a b = Xor [a;b] 

  let r = 
    ( (Name "toto" & Integer)
    ^ ( Name "enum_t" & ( (Name "token11" & Integer) 
		        || (Float & Name "token2" & (Name "ai" ^ Name "bi"))
                        ) 
      )
    ^ ( 
       ( Ref ["toto"] ||  Ref ["enum_t"] || Float) 
       & Name "wierd" & (Name "aw" ^ Name "bw")
      )
	
    )
    ^ (Name "Yo" & Name "Yoyo" & Name "Yi" & (Name "x" ^ Name "y" ^ Name "z"))
    ^ (Name "A" & (Name "a" ^ Name "b"))
    ^ (Name "A" & (Name "a" || Name "b"))
    ^ (Name "Az" & (Name "a" & Name "b"))
    ^ (Name "A" || (Name "a" ^ Name "b"))
    ^ (Name "A" || (Name "a" || Name "b"))
    ^ (Name "A" || (Name "a" & Name "b"))
    ^ (Name "Ai" ^ (Name "a" ^ Name "b"))
    ^ (Name "A" ^ (Name "a" || Name "b"))
    ^ (Name "A" ^ (Name "a" & Name "b"))
    ^ (Name "Xx" ^ Name "Y" ^ Name "Z" ^ Name "T")
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

let ex = And (Name "f", Float)
let ex1 = And (Name "i", Integer)
(*let _ = Tstring.print (to_string r)
let _ = andCheck r r r |> print_error*)
let _ = check r  |> print_error
(*let _ = print_endline "plop : "
let _ = check_to_debug r*)
