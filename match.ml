
(** 
grammar A. sexp like. 
pattern -> "Lit" string
pattern -> "Cat" (pattern, pattern)
pattern -> "Or" (pattern, pattern)
pattern -> "Star" pattern

patern:
| char
| pattern *
| pattern | pattern
| pattern pattern
| (pattern)
 *)

(*let parsepat str = *)

type pattern = Cat of pattern * pattern | Star of pattern |
	       Or of pattern * pattern | Lit of string with sexp

let patmatch pattern (str : string) = 
  let rec pathelp pattern (chars: char list) =  match pattern with 
	| Cat(leftpat, rightpat) -> cathelper leftpat rightpat chars 0
	| Lit(st) -> (String.to_list st) = chars
	| Or(a,b) -> (pathelp a chars) || (pathelp b chars)
	| Star(p) -> starhelper (Lit "") p 0 chars
  and cathelper  left right (chars: char list) pos = 
    if pos > List.length chars then false else 
      if (pathelp right 
		  (List.sub ~pos  ~len:(List.length chars - pos) chars)) && 
	   (pathelp left (List.sub ~pos:0 ~len:pos chars))  then true else 
	cathelper left right chars (pos + 1)
  and starhelper acc p depth (chars : char list)  = 
    if pathelp acc chars then true else
      if depth >= (List.length chars) then false else 
	starhelper (Cat (acc, p)) p (depth + 1) chars
	  in pathelp pattern (String.to_list str)

type testcase = { pat:pattern; str:string; expected:bool } with sexp

let testcases = [
    { pat=Lit ""; str=""; expected=true };
    { pat=Lit ""; str="a"; expected=false };
    { pat=Lit "a"; str="a"; expected=true }; 
    { pat=Lit "a"; str=""; expected=false }; 
    { pat=Cat (Lit "", Lit ""); str=""; expected=true }; 
    { pat=Cat (Lit "", Lit ""); str="a"; expected=false };
    { pat=Cat (Lit "a", Lit "b"); str="ab"; expected=true }; 
    { pat=Cat (Lit "a", Lit "b"); str="ac"; expected=false }; 
    { pat=Or (Lit "", Lit "a"); str=""; expected=true };
    { pat=Or (Lit "", Lit "a"); str="a"; expected=true };
    { pat=Or (Lit "", Lit "a"); str="b"; expected=false };
    { pat=Star (Lit "a"); str=""; expected=true };
    { pat=Star (Lit "a"); str="a"; expected=true };
    { pat=Star (Lit "a"); str="aa"; expected=true };
    { pat=Star (Lit "a"); str="b"; expected=false };
    { pat=Star (Or (Lit "a", Lit "bb")); str=""; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="a"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="bb"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="abb"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="bba"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="aa"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="bbbb"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="abba"; expected=true };
    { pat=Star (Or (Lit "a", Lit "bb")); str="b"; expected=false };
    { pat=Star (Or (Lit "a", Lit "bb")); str="bbb"; expected=false };
    { pat=Star (Or (Lit "a", Lit "bb")); str="bab"; expected=false };
  ];;

let runTest case =
  if (patmatch case.pat case.str) = case.expected then () else
    failwith (Printf.sprintf
		"Failed testcase: %s\n%!"
		(Sexp.to_string (sexp_of_testcase case)))

let () = List.iter testcases ~f:runTest
