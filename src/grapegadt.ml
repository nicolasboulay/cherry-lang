type t = 
  | TLInt of int
  | TLString of string
  | TLReal of float
  | TInteger
  | TString
  | TReal
  | TAnd of t * t (* & *)
  | TXor of t * t (* ^ *)
  | TOr of t *t   (* | *)
  | TSpace of t list (* let a = b c d *)
  | TNamed of string * t

type _ term  = 
  (*Literals*)
  | LInt : int -> int term
  | LString : string -> string term 
  | LFloat : float -> float term
  (*Internal type*)
  | Integer : int term
  | Float : float term
  | String : string term
  (*Op*)
  | And : 'a term * 'a term -> 'a term
  | Or : _ term * _ term ->  _ term 
  | Xor : 'a term list-> _ term (*agregation also used for array*)
  (*Nommé *)
  | Name : string -> _ term
  (*Pointeur avec un path*)
  | Ref : string list -> _ term
  (*Multiplicité*)
  | Mult : int * int -> 'a term

(*Pour des raisons de performances, ilfaudrait manipuler des list de string*)

let sep = " "

let rec string_of_term : type a . a term -> string list =
  fun t ->
  match t with
  (*Literals*)
  | LInt i -> [string_of_int i ]
  | LString s -> ["\"";s;"\""]
  | LFloat f -> [string_of_float f] 
  (*Internal type*)
  | Integer -> ["Integer"]
  | Float -> ["Float"]
  | String -> ["String"]
  (*Op*)
  | And (a,b) -> List.flatten 
	[["("];string_of_term a ; [" and "]; string_of_term b;[")"]]  
  | Or (a,b) ->  List.flatten 
	[["("];string_of_term a ; [" or "]; string_of_term b; [")"]]  
  | Xor [] -> ["(xor)"]
  | Xor (a::[]) -> ["(xor a)"] 
  | Xor (a::l) ->  List.flatten (List.flatten (
      [string_of_term a] :: (List.map (fun s -> [[" xor "] ; string_of_term s])  l) ))  
  (*Nommé *)
  | Name n -> [n]
  (*Pointeur avec un path*)
  | Ref l -> "Ref " :: l 
  (*Multiplicité*)
  | Mult (min,max)-> ["Mult{";string_of_int min;sep;string_of_int max] 

(*vieux repris de la doc*)
type (_,_) eq = Eq : ('a,'a) eq

let cast: type a b . (a,b) eq -> a -> b = 
  fun Eq x -> x

let rec eq_type : type a b. a term -> b term -> (a,b) eq option =
  fun a b ->
    match a, b with
    | Integer, Integer -> Some Eq
    | Float, Float -> Some Eq
    | _ -> None

let _ = 
  eq_type Integer Float
(********************************)

(*c'est commutatif, cela permet de réduire les cas*)
let rec andCheckOne : type a b. a term -> b term -> bool =
  fun a b ->
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
  | a, And (b,c) -> (andCheckOne a b) && (andCheckOne a c)
  | a, Or (b,c) -> print_string "Or ";(andCheckOne a b) || (andCheckOne a c)
  | Xor [] , Xor [] -> true  
  | Xor (a::c) , Xor (b::d) -> andCheckOne a b && andCheckOne (Xor c) (Xor d)
  | Name _ , _ -> true
  | Ref _ , _ -> true (*il faudrait tester avec la destination*)
  | Mult (min, max), Mult (min2, max2) -> min < min2 && max2 < max
  | _ -> false
    in
    if(r) then print_string "t" 
    else print_string "f";
    r
let andCheck : type a b. a term -> b term -> bool =
  fun a b ->
    (andCheckOne a b ) || (andCheckOne b a )

let check a =
  match a with 
  | And (a,b) -> andCheck a b
  | _ -> true

(*let check a =
  match a with 
  | And (Or (a,b), c)-> true (* (same_type a c) || same_type b c *)
  | Or (e, f)-> true (*(same_type e f) *)(*|| same_type b c*) 
	(*| And (c, (Or (a,b))) -> same_type a c || same_type b c *)
  | _ -> true    
*)

let (&): type a. a term -> a term -> a term = fun a b ->  And (a, b)
let (||) a b  =   Or (a, b) 
let (^) a b = Xor [a;b] 

let ta = TNamed ("toto", (TAnd (TLInt 1, TInteger)))
let tb = TNamed ("toto", (TAnd (TLInt 1, TReal))) (* false *)


let a = And (LInt 1, Integer)
let aa = (LInt 1) & Integer
let b = And (LFloat 1.0, Float)
let bb = (LFloat 1.0) & Float
let dd = Integer || Float
let c = Or (a ,Integer)
let cc = ( a || Integer ) & Float
(*let d = (b ^ a) & (b ^ a)*)
(*let e = (b ^ a) & (a ^ b)*)

(*let f = Space f1*)
(*let b = And (Lit 1, Float) (*false*)*)

let g = LInt 1 &  ( LString "toto" || Integer)


let tt a =
  if check a then 
    print_string "ok\n" 
else
    print_string "false\n" 

let _ = check a

let print_term t =
  t |> string_of_term |> String.concat "" |> print_endline ;; 


let _ = 
  tt a;; tt b;; tt g;; 
   print_term a;; print_term b;; print_term g;;print_term c;; print_term cc




