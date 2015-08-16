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
	Tstr [ to_string_ st t a; Str " "; to_string_ st t b]
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

let (&) a b = And (a, b)
let (||) a b = Or (a, b) 
let (^) a b = Xor [a;b] 

  let r = ((Name "toto" & Integer)
    ^ (Name "enum_t" & ((Name "token1" & Integer) 
		      || (Float & Name "token2" & (Name "a" ^ Name "b"))) )
    ^ ((Ref ["toto"] || Float || Ref ["enum_t"]) 
       & Name "wierd" & (Name "a" ^ Name "b")))
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
let _ = Tstring.print (to_string r)
