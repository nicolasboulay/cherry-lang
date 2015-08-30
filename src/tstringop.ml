type tstring = (* to avoid recopy better than string list *)
  | Tstr of tstring list
  | Str of string

