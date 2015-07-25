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

let rec to_string t =
  let to_string_simple t =
    match t with
    | LInt i  -> Str (string_of_int i )
    | LString s  -> Tstr [Str "\""; Str s; Str "\""] 
    | LFloat f -> Str (string_of_float f ) 
    | Integer -> Str "Integer"
    | Float ->  Str "Float"
    | String -> Str "String"
    | Name n -> Tstr [Str "let "; Str n]
    | Ref (h::l) -> Tstr ((Str h) ::
	( List.fold_right (fun (e:string) (a:tstring list)  -> (Str "/") :: (Str e) :: a ) l  []  ))
    | Mult (min,max) -> Tstr [Str "{"; Str (string_of_int min); sep; Str (string_of_int max); Str "}"] 
    | _ -> Tstr []
  in
  to_string_simple t 

let ex = Integer
let _ = to_string ex
