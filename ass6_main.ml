
open Ass6_backend


(* Function to get set of clauses from an input file which is taken as parameter by the program .  *)
let rec getClauses str = 
   let temp = ref [] in 
	   try
	      	let in_stream = open_in str in 
			    let lexbuf = Lexing.from_channel in_stream in
			    while true do
			      let input = Parser_ass6.main Lexer_final.token lexbuf in 
			      	let (head,body) = input in if(List.length head > 1) then raise InvalidInput else temp := input::(!temp) ;
			    done ;
			    (!temp) ;
	    with Lexer_final.Eof ->
	    	Printf.printf "\n-------------Done with getting clauses---------------\n\n" ;
	    	(!temp) ;;

(* Main program that takes in queries, passes them to the function answerQueries in ass6_backend.ml and prints true or false as returned by the function . 
It keeps running till the list of queries runs out or an invalid query is detected by the parser .  *)
let _ =
	let clauses_temp = getClauses Sys.argv.(1) in
		let clauses = (List.map tweakClause clauses_temp) in 
		Printf.printf "\n-------------Now getting queries---------------\n\n" ; flush stdout ;
		try
	        let lexbuf = Lexing.from_channel stdin in
	    	while true do
	         let input = Parser_ass6.main Lexer_final.token lexbuf in 
				let (head,body) = input in if(List.length body > 0) then raise InvalidInput else (

					let (head2,_) = tweakQueries head 0 in
					let (flag,flag2) = (answerQueries head2 clauses [] clauses 0 100 false) in 
						Printf.printf "%b;\n\n" flag;flush stdout;)
	        done ;
	      with Lexer_final.Eof ->
	        exit 0

