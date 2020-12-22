

{ 
open Parser_ass6
open Ass6_backend
exception Eof
}

(* Set of regex to detect tokens such as float indices , ranges etc.*)
let digit = ['0'-'9']
let num = ['1'-'9']
let sign = ['+' '-']

(* Regex for integer*)
let ind_reg ="0" | (num digit*) 
(*Regex for part of float after the decimal point*)
let dec_reg = (digit* num) | "0"
(* Complete Regex for float whcih included optional sign*)
let float_str = sign?ind_reg"."dec_reg
(*Regex for white space that is useful in detecting tokens of indices and ranges*)
let ws = [' ' '\t']

let reg1 = ['A'-'Z' 'a'-'z' '_' '0'-'9'] 

(* Set of rules to parse the tokens . The tokens for indices and ranges covers the possibility that there are spaces in between some of the parts as well . 
In case of wrong token ,lexing error is raised by the program*)
rule token = parse
	| [ ' ' '\t' '\n'] {token lexbuf}
	| "("          {LEFCL}
	| ")"          {RGTCL}
	| ","           {COMMA}
	| "."           {TERM}
	| ";"			{CONTINUE}
	| ":-"			{ASSIGN}
	| (['A' - 'Z'] reg1*) as var  {VARIABLE(var)}
	| (['a' - 'z'] reg1*) as var  {CONSTANT(var)}
	| "_"                  	      {VARIABLE("_")}       
 	| eof { raise Eof }

{

(*
(* Main function to generate Output *)
let main () = begin
	try
	let lexbuf = Lexing.from_channel stdin in
	while true do 
		let result = token lexbuf in
			match result with
		     TERM -> Printf.printf(" (Termination) ")
		   | RGTCL -> Printf.printf(" (Right rounded bracket) ")
		   | LEFCL -> Printf.printf(" (Left rounded bracket) ")
		   | COMMA -> Printf.printf(" (Comma) ")
		   | ASSIGN -> Printf.printf(" (Assignment) ")
		   | CONTINUE -> Printf.printf(" (Continue) ")
		   | VARIABLE(v) ->  Printf.printf "  (Variable :  %s)  " v
		   | CONSTANT(v) ->  Printf.printf "  (Constant : %s)   " v
		   | _ -> Printf.printf(" (Unknown Token) ")
	done
	with Eof -> exit 0
end ;;
main();;
*)
}
