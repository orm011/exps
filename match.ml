type pattern = Cat of pattern * pattern | Star of pattern |
	       Or of pattern * pattern | Lit of string with sexp;;

type parsecase = { pat:string; expected:pattern } with sexp;;

let parsecases =  [
    { pat="a"; expected=Lit "a" };
    { pat="ab"; expected=Cat (Lit "a", Lit "b") };
    { pat="abc"; expected=Cat (Lit"a", Cat (Lit "b", Lit "c")) };
    { pat="a|b"; expected=Or (Lit "a", Lit "b") };
    { pat="a|b|c"; expected=Or (Lit "a", Or (Lit "b", Lit "c")) };
    { pat="a*"; expected=Star (Lit "a") };
    { pat="ab*"; expected=Cat (Lit "a", Star (Lit "b")) };
    { pat="a|bc"; expected=Or (Lit "a", Cat (Lit "b", Lit "c")) };
    { pat="(ab)*"; expected=Star ( Cat (Lit "a", Lit "b") ) }
  ];;


(*
root -> allpat []
allpat -> starcat 
allpat -> starcat | allpat 
starcat -> starp
starcat -> starp starcat 
starp -> simplep
starp -> simplep *
simplep -> simple char
simplep -> (allpat)

after(allpat) =  [], )
after(starcat): after(allpat) U  |
after(starp):  after(starcat) U  begin(starcat)
after(simplep): after(startp) U *
*)

let parsepat (str : string) : pattern = 
  let debug_str (name: string) (str: char list) = 
    Printf.printf "Debug at %s: %s\n%!"  name 
		   (Sexp.to_string  (sexp_of_list sexp_of_char str)) in    
  let err_str (site :string) (str : char list)  =
    Printf.sprintf 
      "(context %s) unexpected rem=%s" 
      site (Sexp.to_string  (sexp_of_list sexp_of_char str)) in
  let rec allpat (str : char list) : pattern * char list = 
    let (pat, rem) = starcat str in 
    match rem with 
    | [] | ')' :: _ -> (pat, rem)
    | '|' :: rem2 -> let (secondpat, rem3) = allpat rem2 in 
		  (Or(pat, secondpat), rem3)
    | x -> failwith (err_str "1" rem)
  and starcat (str : char list) : pattern * char list = 
    let (pat, rem) = starp str in 
    match rem with 
    | [] | (')' | '|'):: _  -> (pat,rem)
    | _ -> let (secondpat, rem2) = starcat rem in 
	   (Cat(pat, secondpat), rem2)
  and starp (str: char list) : pattern * char list  =
    let (pat, rem) = simplep str in 
    match rem with 
    | '*':: rem2 -> (Star(pat), rem2)
    | _ -> (pat, rem)
  and simplep (str: char list) : pattern * char list = 
    match str with 
    | '(' :: rem -> let (pat, rem2) = allpat rem in
		    (match rem2 with 
		     | ')':: rem3 -> (pat, rem3)
		     | _ -> failwith (err_str "2" rem) )
    | x :: rem -> (Lit (Char.to_string x), rem)
    | [] -> failwith "expected more input"
  in let (pat, rem) = allpat (String.to_list  str) in 
     match rem with
	 | [] -> pat
	 | _ -> failwith (err_str "3" rem)


						    
let runparsetest (case : parsecase)  =
  if (try parsepat case.pat with
      | e -> failwith (Printf.sprintf
			 "Exception %s thrown. Failed parse testcase: %s\n%!"
			 (Sexp.to_string (sexp_of_exn e)) (Sexp.to_string (sexp_of_parsecase case))))
     = case.expected then () else
    failwith (Printf.sprintf
		"Failed parse testcase: %s\n%!"
		(Sexp.to_string (sexp_of_parsecase case)))

let () = List.iter parsecases ~f:runparsetest

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


let matches (pat:string) (str:string) =
  patmatch (parsepat pat) str
