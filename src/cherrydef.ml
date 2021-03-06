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
  | And of t * t (* ~ or ~=, type equality*)
  | Or  of t * t (*typical || of sum type*)
  | Xor of t list (* typical * of sum type*)
	(* Name of node*)
  | Name of string 
	(* Pointer with path name, not pointer *)
  | Ref of string list (* TODO: adding integer index ref, + reference  and relative ../ as directory *)
	(* Multiplicity *)
  | Mult of int * int (*min/max*)
(* | Not of t TODO: to complete the logical of the type *)
(* | Unordered : to check contener without order constraint : interresting for overloading/inheritance*)
(* | Tag of string: like name but no alias possible, behave like type name, for properties check*)

open Tstringop

let sep = Str " " (*this is to avoid many " " string in the binary*)

(*
  stringyfication of a the main type, it's pretty hard to do it correclty,
  some '\n' are still present. Return a "Str" tree type for ultra fast, string manipulation. 
  It will need cleaning in the long term.
*)
let to_string t =
  let to_string_simple t =
    match t with
    | LInt i  -> Str (string_of_int i )
    | LString s  -> Tstr [Str "\""; Str s; Str "\""] 
    | LFloat f -> Str (string_of_float f ) 
    | Integer -> Str "Integer"
    | Float ->  Str "Float"
    | String -> Str "String"
    | Name n -> Tstr [Str ""; Str n; Str "="]
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
				     Str "\n";st;Str "* "; to_string_ st t (Xor tl) ;]
    | Xor (a::[]), _, _ -> Tstr [Str "\n";st; Str "* "; to_string_ st t a;]  
    | Xor (a::tl), _, _ -> Tstr [Str "\n";st; Str "* "; to_string_ st t a;  
				 Str "\n";st; Str "* "; to_string_ st t (Xor tl) ;] 
    | Xor [], _, _ -> Str ""
    | a, _ , _ -> to_string_simple a 
  in
  to_string_ (Str "") (Name "root") t 

(*let print_bool b =
  if b then 
    print_string "ok" 
  else
    print_string "false" 
*)
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
   * /!\ Ref case need a root for reference, partial checking could be done, but the root 
   should be given to find all reference.
   if not, a missing reference should not be a mistake, but the input are still invalid. 
*)
type error = 
  | No
  | BasicTypeMismatch of t  * t list * t * t list *t (*path_and, path a, node a, path b, node b*)
  | UnknownReference of t list * t
  | IncompatibleMultiplicity of t * t list * t * t list * t
  | Multiplicity of t  * t list * t * t list * t
  | Other of t  * t list * t * t list * t 

(*
  Debug function to pretty print error 
*)
let print_error e =
  let print_path p =
    List.iter ( fun ref->Tstring.print (to_string ref); print_endline "" ) p
  in
  let p_ patha a pathb b =
    print_path patha; 
    Tstring.print (to_string a); 
    print_endline "\n---";
    print_path pathb; 
    Tstring.print (to_string b); print_endline "" 
  in
  match e with 
  | No -> print_string "No error"
  | UnknownReference(path, a) -> 
      print_endline "Unknown reference :"; 
      print_path path;
      Tstring.print (to_string a)
  | BasicTypeMismatch (path_and, patha, a, pathb, b)->
      Tstring.print (to_string path_and);  
      print_endline "Basic type mismatch :"; 
      p_ patha a pathb b
  | IncompatibleMultiplicity (path_and, patha, a, pathb, b) -> 
      Tstring.print (to_string path_and);  
      print_endline "Incompatible multiplicity"; 
      p_ patha a pathb b
  | Multiplicity (path_and, patha, a, pathb, b) ->
      Tstring.print (to_string path_and);   
      print_endline "Wrong multiplicty"; 
      p_ patha a pathb b
  | Other (path_and, patha,a,pathb, b) -> 
      Tstring.print (to_string path_and);  
      print_endline "Unkown error "; 
      p_ patha a pathb b


(* kind of lazy evaluation for (much much : x1000) faster resolution*)
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

(**return the name of node*)
(* TODO: should return a type t, to combine name and alias (Name "a" & Name "alias1") *)
let rec getName_ a = 
  let or_ a b =
    match a with 
    | None -> b ()
    | _ -> a
  in
  match a with
  | And (b,c) -> or_ (getName_ b) ( fun () -> getName_ c ) 
  | Name s -> Some s
  | _ -> None
and getName a =
  match getName_ a with 
  | None -> Name "noname"
  | Some n -> Name n

(* c'est commutatif, cela permet de réduire les cas *)
(*Check is Commutative, to reduce the number of check and for clarity.
  Check is also associative (A * B * C = (A*B) * C = A * (B*C)).
*)
let rec andCheckOne aa bb root pathaa pathbb path_and =
  let check_ expr error =
    if expr then
      error
    else 
     No
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
    | LString s1, LString s2 -> check_ (s1 == s2) (BasicTypeMismatch (path_and,pathaa,aa,pathbb,bb)) (*tbc*)
    | LFloat f1, LFloat f2 -> check_ (f1 == f2) (BasicTypeMismatch (path_and,pathaa,aa,pathbb,bb))
    | LInt i1, LInt i2 -> check_ (i1 == i2)  (BasicTypeMismatch (path_and,pathaa,aa,pathbb,bb))
    | Name s , _ -> print_string "Name "; print_endline s; 
	check_with_root_ bb root pathbb path_and  
    | Ref path_ , b ->
	let refr = find path_ root in (*todo: same name at different place must be catch*)
	Tstring.print (to_string aa); print_string " -> "; 
	( match refr with
        | Some node -> Tstring.print (to_string node) ; print_endline "";
	    andCheck_ node b root (aa::pathaa) pathbb path_and 
	| None -> print_endline "<unknownref>" ; 
	    UnknownReference (pathaa,aa) (*? unbound reference are always a bug ?*)
	)
    | a, And (b,c) -> print_endline "a,And";
	((andCheck_ a b root pathaa pathbb path_and) &&& (fun () -> andCheck_ a c root pathaa pathbb path_and))
	 &&& (fun () -> andCheck_ b c root pathbb pathbb path_and) 
    | a, Or (b,c) ->  (andCheck_ a b root pathaa pathbb path_and) 
      ||| (fun () -> andCheck_ a c root  pathaa pathbb path_and)
    | Xor [] , Xor [] -> print_endline "Xor,Xor"; No  
    | Xor (a::c), Xor (b::d) -> andCheck_ a b root pathaa pathbb path_and 
	          &&& (fun() -> andCheck_ (Xor c) (Xor d) root pathaa pathbb path_and)
    | Mult (min, max), Mult (min2, max2) -> (* l'un est inclus dans l'autre *)
	check_ (min <= min2 && max2 <= max) (IncompatibleMultiplicity (path_and,pathaa, aa, pathbb, bb))
    | Mult (min,max), Xor l -> (**xor : array, composition*)
	let rec check_array min max ll =
	  (*print_string "check_array "; print_int min ; 
	    print_string " "; print_int max; print_endline "";*)
	  match ll with (*! lent : o(n)*) 
	  | [] -> check_ (min <= 0 && 0 <= max) (Multiplicity (path_and,pathaa, aa, pathbb, bb))
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
    | String,  _ -> print_endline "mismatch";(BasicTypeMismatch (path_and, pathaa, aa, pathbb, bb))
    | _ -> print_endline "Other "; Other (path_and, pathaa, aa, pathbb, bb)
  in
  (*print_bool r; print_string ")";*)
  r

and andCheck_ a b root patha pathb path_and =
  (andCheckOne a b root patha pathb path_and ) ||| ( fun () -> andCheckOne b a root pathb patha path_and)

(*and andCheck a b root =
  andCheck_ a b root [] []
 *)
and check_with_root_ node root path_node path_and : error=
  let name = getName node in
  match node with 
  | And (a,b) -> andCheck_ a b root path_node path_node (name)(*same origin, same path*)
  | Or (a,b)  -> 
      check_with_root_ a root path_node name
	&&& ( fun () -> check_with_root_ b root path_node name )
  | Xor l -> List.fold_left (fun a t -> a &&& (fun() -> check_with_root_ t root path_node name)) No l
  | _ -> No

and check_with_root node root : error= 
  check_with_root_ node root [] (Xor[])
 
and check root =
  check_with_root root root


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
  (*let print_ node root =
    print_string "Type error : ";
    to_string node |> Tstring.print; print_string "\n";
    print_endline " Dependancies : ";

    print_dependancies node root;   
  in*)
  match check_with_root node root with
  | No -> ()
  | e -> print_error e;
    print_endline " Dependancies : ";
    print_dependancies node root   
  (*| UnknownReference (_,a) -> print_ a root
  | BasicTypeMismatch (_,a,b)
  | IncompatibleMultiplicity (_,a,b)
  | Multiplicity (_,a,b)
  | Other (_,a,b) -> print_ a root; print_ b root	
*)
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

  let r = Name "presqueroot" & 
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
let rr = ex ^ ex1 ^ ex11 ^ ex12 ^ (Name "plop" & Ref ["ff"] & Ref ["ii"])
let _ = Tstring.print (to_string r);print_endline ""
(*let _ = andCheck r r r |> print_error*)
let _ = check r |> print_error
let _ = print_endline "getname :"
let _ = getName r |> to_string |> Tstring.print
(*let _ = print_endline "plop : "
let _ = check_to_debug r
*)
