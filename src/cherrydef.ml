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
  | BasicTypeMismatch of t list * t*t
  | UnknownReference of t list * t
  | IncompatibleMultiplicity of t list * t * t
  | Multiplicity of t list * t * t
  | Other of t list * t * t(*path a, node a, node b*)

let print_error e =
  let print_path p =
    List.iter ( fun ref->Tstring.print (to_string ref); print_endline "" ) p
  in
  let p_ a b =
    Tstring.print (to_string a); 
    print_endline "\n---";
    Tstring.print (to_string b); print_endline "" 
  in
  match e with 
  | No -> print_string "No error"
  | UnknownReference(path, a) -> print_path path;print_endline "Unknown reference :"; 
      Tstring.print (to_string a)
  | BasicTypeMismatch (path, a, b)-> print_path path; print_endline "Basic type mismatch :"; p_ a b
  | IncompatibleMultiplicity (path, a, b) -> print_path path; 
      print_endline "Incompatible multiplicity"; p_ a b
  | Multiplicity (path, a, b) -> print_path path; print_endline "Wrong multiplicty"; p_ a b
  | Other (path,a,b) -> print_endline "Error "; print_path path; p_ a b

(* kind of lazy evaluation for (much much x1000) faster resolution*)
let ( ||| ) a b = 
  match a with 
  | No -> No
  | _ -> let b_ = b() in
    ( match b_ with
    | Other _ -> a
    | _ -> b_ 
     )

let ( &&& ) a b = 
  match a with 
  | No -> b ()
  | _ -> a





(* c'est commutatif, cela permet de réduire les cas *)
let rec andCheckOne aa bb root path =
  let check_ expr error =
    if expr then
      error
    else 
     No
  in
  let rec getName a = (* should return a type t, to combine name (Name "a" & Name "alias1") *)
    let or_ a b =
      match a with 
      | None -> b ()
      | _ -> a
    in
    match a with
    | And (b,c) -> or_ (getName b) (fun () -> getName c) 
    | Name s -> Some s
    | _ -> None
  in
  
  print_string "(";
  Tstring.print (to_string aa); print_string " == ";Tstring.print (to_string bb) ;
  print_string ")\t";
  let r =
    match aa,bb with
    | Integer, Integer 
    | Float, Float 
    | String, String
    | Integer, LInt _
    | Float, LFloat _ 
    | String, LString _ -> No
    | LString s1, LString s2 -> check_ (s1 == s2) (BasicTypeMismatch (path,aa,bb)) (*tbc*)
    | LFloat f1, LFloat f2 -> check_ (f1 == f2) (BasicTypeMismatch (path,aa,bb))
    | LInt i1, LInt i2 -> check_ (i1 == i2)  (BasicTypeMismatch (path,aa,bb))
    | Name s , a -> print_string "Name "; print_endline s; 
	check_with_root a root  
    | Ref path_ , b ->
	let refr = find path_ root in (*todo: same name at different place must be catch*)
	Tstring.print (to_string aa); print_string " -> "; 
	( match refr with
        | Some node -> Tstring.print (to_string node) ; print_endline "";
	    andCheck_ node b root (aa::path) 
	| None -> print_endline "<unknownref>" ; UnknownReference (path,aa) (*? unbound reference are always a bug ?*)
	)
    | a, And (b,c) -> print_endline "a,And";
	((andCheck_ a b root path ) &&& (fun () -> andCheck_ a c root path ))
	 &&& (fun () -> andCheck_ b c root path ) 
    | a, Or (b,c) ->  (andCheck_ a b root path) ||| (fun () -> andCheck_ a c root path )
    | Xor [] , Xor [] -> print_endline "Xor,Xor"; No  
    | Xor (a::c), Xor (b::d) -> andCheck_ a b root path 
	          &&& (fun() -> andCheck_ (Xor c) (Xor d) root path)
    | Mult (min, max), Mult (min2, max2) -> (* l'un est inclus dans l'autre *)
	check_ (min <= min2 && max2 <= max) (IncompatibleMultiplicity (path,aa,bb))
    | Mult (min,max), Xor l -> (**xor : array, composition*)
	let rec check_array min max ll =
	  (*print_string "check_array "; print_int min ; 
	    print_string " "; print_int max; print_endline "";*)
	  match ll with (*! lent : o(n)*) 
	  | [] -> check_ (min <= 0 && 0 <= max) (Multiplicity (path, aa,bb))
	  | (Xor b) :: [] -> check_array min max b 
	  | (Xor b) :: l  -> check_array min max (b@l) 
	  | Or (b,c) :: l -> (check_array min max (b::l)) ||| (fun () -> check_array min max (c::l))
 	  | And (b,c) :: l ->  (check_array min max (b::l)) &&& (fun () -> check_array min max (c::l))
	  | a :: l -> check_array (min-1) (max-1) l
	in
	check_array min max l
    | LInt _, _ 
    | LFloat _, _ 
    | LString _, _ 
    | Integer, _ 
    | Float,  _ 
    | String,  _ -> print_endline "mismatch";(BasicTypeMismatch (path, aa,bb))
    | _ -> print_endline "Other "; Other (path,aa,bb)
  in
  (*print_bool r; print_string ")";*)
  r
and andCheck_ a b root path  =
  (andCheckOne a b root path ) ||| ( fun () -> andCheckOne b a root path )
and andCheck a b root =
  andCheck_ a b root [] 
and check root =
  check_with_root root root
and check_with_root node root = 
  match node with 
  | And (a,b) -> andCheck a b root
  | Or (a,b) -> check_with_root a root &&& (fun () -> check_with_root b root)
  | Xor l -> List.fold_left (fun a t -> a &&& (fun() -> check_with_root t root)) No l
  | _ -> No





(** reread everything below*)


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
  | UnknownReference (_,a) -> print_ a root
  | BasicTypeMismatch (_,a,b)
  | IncompatibleMultiplicity (_,a,b)
  | Multiplicity (_,a,b)
  | Other (_,a,b) -> print_ a root; print_ b root	
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
let ex11 = And (Name "ii", Ref["i"])
let ex12 = And (Name "ff", Ref["f"])
let ex2 = Xor [ex; ex1; ex11; ex12; And (Ref ["ff"] , Ref ["ii"]) ]
let r = ex ^ ex1 ^ ex11 ^ ex12 ^ (Name "plop" & Ref ["ff"] & Ref ["ii"])
let _ = Tstring.print (to_string r);print_endline ""
(*let _ = andCheck r r r |> print_error*)
let _ = check r  |> print_error
(*let _ = print_endline "plop : "
let _ = check_to_debug r
*)
