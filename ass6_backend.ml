
(* Unary type operation. Seperate declaration is useful so that we can group operation together during parsing and have similar type checks for them. *)
type unatemp = 
 	| COUNT
	| ROWCOUNT
	| COLCOUNT
	| SUM
	| ROWSUM
	| COLSUM
	| AVG
	| ROWAVG
	| COLAVG
	| MIN
	| ROWMIN
	| COLMIN
	| MAX
	| ROWMAX
	| COLMAX 
	| PRINT ;;


(* Unary type operation. Seperate declaration is useful so that we can group operation together during parsing and have similar type checks for them *)
type bitemp = 
	| ADD
	| SUBT
	| MULT
	| DIV ;;

type symbol = Symb of string ;;
type variable = Var of string ;;

type term = V of variable | Node of symbol * (term list);;

(* Representation of a single term of substitution list as a tuple  and denoting substitution as a list of such tuples*)
type substTerm =  Subst of variable*term  ;;
type substitution = substTerm list ;;


type clause = (term list)*(term list) ;;

type program = clause list ;; 


(* Different Exceptions suitably defined *)
exception InvalidInput;;
exception InvalidRange ;;
exception InvalidQuery ;; 
exception UndefinedOperationError ;;
exception ComposeError ;;
exception NOT_UNIFIABLE ;;
exception InvalidClause

let rec printTerm x = 
						let rec aux tl = match tl with
								[] -> () ;
							  | t::ts -> ((printTerm t) ; (aux ts) ;) in
						match x with
					Node(Symb(st1),[]) -> Printf.printf  "\tConstant :  %s \n" st1 ;
					| V(Var(st1)) -> Printf.printf  "\tVariable :  %s \n" st1 ;
					| Node(Symb(st1),tl) -> Printf.printf  "\tConstant with many terms in term list :  %s  %d\n" st1 (List.length tl) ; aux tl ;;

(* Function for comparing 2 symbols s1 and s2 so that this can be used for sorting in the function checkSig.
	if s1 is lexicographically smaller then s2 , it returns -1 else it returns 1 *)
let compareSymb s1 s2 = let (a,b) = s1 and (c,d) = s2 in
							let Symb(string1) = a and Symb(string2) = c in
								if(string1 > string2) then 1 else -1 ;;



(* Function for comparing 2 substituion s1 and s2 so that this can be used for sorting in the function composeSubstitutions.
	if s1 is lexicographically smaller then s2 , it returns -1 else it returns 1 *)
let compareSubst s1 s2 = let Subst(a,b) = s1 and Subst(c,d) = s2 in
							let Var(string1) = a and Var(string2) = c in
								if(string1 > string2) then 1 else -1 ;;


(* Function to check if 2 symbols are equal to be used in checkSig *)
let areEqual s1 s2 = let (a,b) = s1 and (c,d) = s2 in
							let Symb(string1) = a and Symb(string2) = c in
								if(string1 == string2) then true else false ;;



(*  Function to check if signature denoted by list l1 is a valid signature. We sort the list first so that complexity 
    becomes O(nlogn) rather than n^2 when we need to check for repeated symbols.Further we also check for arity being positive.
    If it is valid signature we return true else we return false. *)
let checkSig l1 = let sorted_l = (List.sort compareSymb l1) in
					let rec aux l1 = match l1 with
						[] -> true
						| x::[] -> true
						| x1::x2::xs -> if(areEqual x1 x2) then false else (aux (x2::xs)) in
					let rec aux2 l1 = match l1 with
						[] -> true
						| x::xs -> let (a,b) = x in if(b < 0) then false else (aux2 xs) in
					(aux sorted_l) && (aux2 sorted_l) ;;



(* Provides the arity of Symbol denoted by symb given a valid signature.It traverses the list till 
	it find the required symbol in the signature list and then returns the arity.. *)
let rec getArity signature symb = match signature with
							[] -> -1
						   | x::xs -> let (a,b) = x in let Symb(string1) = a and Symb(string2) = symb in
						   					let temp = print_string string1 and temp2 = print_string string2 in
						   					if(string1 =  string2) then b else (getArity xs symb) ;;


(* Given a list of booleans we return true if all are true else false. Used in wfterm function *)
let rec checkFlags l = match l with
				[] -> true
				| x::xs -> x&&(checkFlags xs) ;; 



(* Function checks if the term t is valid given a valid signature . It does so recursively by checking 
  if given a non-leaf Node , does its term list have same number of terms as denoted in the signature . 
  If yes then it checks the child nodes and if any of them are invalid it return false. For a variable term , always True is returned*)
let rec wfterm signature t = match t with
					V(_) -> true
				   | Node(a,b) -> let ar = (getArity signature a) and len_list = (List.length b) in 
				   					if(ar = len_list) then (
				   							let flags = (List.map (wfterm signature) b) in (checkFlags flags) ;) else false ;; 
				   								



(*  Given a term t it provides the height of the term. For base case , variable has height 0. *)
let rec ht t = match t with
		V(_) -> 0
		| Node(a,b) -> (List.fold_right max (List.map ht b) (-1)) + 1 ;;



(*  Given a term t , it provides the number of nodes both leaf and non-leaf if we imagine the term to be a tree.
    In the base case, a variable has height 1 *)
let rec size t = match t with
		V(_) -> 1
		| Node(a,b) -> (List.fold_right (+) (List.map size b) 0) + 1 ;;



(* Function to insert variable v to a sorted list of variables l such that if v is already present in the list we
   do nothing. Otherwise we insert it into the list so that it remains sorted. Used in the helper function mergeLists *)
let rec insertUnique l v= match l with
		[] -> [v]
		| x::xs -> let Var(v1) = x and Var(v2) = v in if (v1 > v2) then v::l else if(v1 = v2) then l else x::(insertUnique xs v) ;;



(* Given 2 lists l1 and l2 , we merge them so the list remains sorted and no duplicates exist in the list. It uses 
   the function insertUnique to add a singular term to the list. It is used in function vars to 
	merge list of variables obtained from different branches. *)
let rec mergeLists l1 l2 = match l1 with
		| [] -> l2
		| x::xs -> (mergeLists xs (insertUnique l2 x)) ;;



(*  Given a term t , it provides the number of variables in the term in a recursive manner *)
let rec vars t = match t with
		V(var) -> [var]
		| Node(a,b) -> (List.fold_right mergeLists (List.map vars b) []) ;;



(* It inserts a single substitution term st , to the list of substitutions denoting a function such that the list sl remains sorted
   and returns the sorted list denoting a new function.It also raises error ComposeError if the substition for a given
   variable already exists in the list sl. Acts as a helper function to the function composeSubstitutions. *)
let rec insertSubst (st:substTerm) (sl:substitution) : substitution = match sl with
		[] -> [st]
		| x::xs -> let Subst(v1,t1) = x and Subst(v2,t2) = st in let Var(string1) = v1 and Var(string2) = v2 in
			if (string1 > string2) then st::sl else if(v1 = v2) then (Printf.printf  "Var1 :  %s Var2 : %s\n" string1 string2 ; printTerm t1 ; printTerm t2; raise(ComposeError);) else x::(insertSubst st xs) ;;



(* This function takes as input a variable and a substitution function denoted by a list sl, returning the appropriate term
   that we get from the substitution function for the variable v . Acts as a helper function to the function subst.*)
let rec applySubstToVar (v:variable) (sl:substitution) : term = match sl with
										[] -> V(v)
									   | x::xs -> let Subst(v_subst,t_subst) = x in let Var(string1) = v and Var(string2) = v_subst in
									   		if(string1 = string2) then t_subst else (applySubstToVar v xs) ;;


(* Recursive function that takes as input  a term t and a substitution function denoted by a list sl and applies the 
   substitution to the term t and returns the term after applies all the substitutions.*)
let rec subst (sl:substitution) (t:term) : term = match t with
		V(v) -> (applySubstToVar v sl)
		| Node(s,l) -> Node(s,    (List.map (subst sl) l)) ;;


(* Function to compose 2 substitutions s1 and s2 such that if g denotes function s2 and f denotes function s1, then
   we return gof. Since the representation of functions here is as a list , therefore in tune with that idea we return
   a list as well. This is a useful helper function for function mgu  *)
let rec composeSubstitutions (s1 :substitution) (s2 :substitution) : substitution = match s1 with
								[] -> (List.sort compareSubst s2)
								| x::xs -> let Subst(v,t) = x in (insertSubst (Subst(v,(subst s2 t))) (composeSubstitutions xs s2)) ;; 

(* Function to check if term v1 is in list vl. This is used to raise NOT_UNIFIABLE error in the function mgu. *)
let rec isInList v1 vl = match vl with
				[] -> false
				| x::xs -> let Var(s1) = v1 and Var(s2) = x in if(s1 = s2) then true else (isInList v1 xs) ;;

(* Function to find mgu of the two terms t1 and t2 and return 
	1. Whether they are unifiable as true or false
	2. The suitable substitution function as a list of (variable,term).
   We do so in a recursive manner where given a non-leaf node , we find substitutions for left-most child term and
   apply those to the rest of the child terms. Then we find substitutions for rest of the tree and then compose the two 
   substitutions obtained from the rest of the tree and the left-most child term. 
   For this we use the helper function composeSubstitutions. 
   If the two terms are not unifiable we return (false,[])*)
let rec mgu t1 t2 = match t1,t2 with
	| V(v1),V(v2) -> if(v1 = v2) then (true,[]) else (true,[Subst(v1,t2)])
	| V(v1),Node(s,tl) -> let vars_temp = (vars t2) in 
											if(isInList v1 vars_temp) then  ( (false,[]) ;)
											else (true,[Subst(v1,t2)])
	| Node(s,tl),V(v1) -> let vars_temp = (vars t1) in 
											if(isInList v1 vars_temp) then ( (false,[]) ;)
											else (true,[Subst(v1,t1)])
	| Node(s1,tl1),Node(s2,tl2) -> let Symb(string1) = s1 and Symb(string2) = s2 in 
									if ((string1 = string2) && ((List.length tl1) = (List.length tl2))) then (
										match tl1,tl2 with
											[],[] ->  (true,[]) ; 
											| x::xs,y::ys -> let (flag,subst_list) = (mgu x y) in 
																if(flag) then (
																let xs_subst = (List.map (subst subst_list) xs) and 
																	ys_subst = (List.map (subst subst_list) ys) in
																		let (flag2,subst_list2) = (mgu (Node(s1,xs_subst))  (Node(s2,ys_subst))) in
																			 if (flag2) then (true,(composeSubstitutions subst_list subst_list2)) else (false,[]) ;
																	)
																else  ( (false,[]) ;)  ;
										)
									else  ( (false,[]) ;) ;;


(* Function to print a list of terms . Made essentially for purposes of testing. Acts as helper function to printAtom . *)
let rec printTerms tl = match tl with
					[] -> Printf.printf "Empty List\n" ;
					| x::[] ->  printTerm x ;
					| x::xs -> printTerm x ; printTerms xs ;;

(* Used to print a single atom of a list of clauses . Acts as helper function to printBody *)
let rec printAtom atom = let Node(Symb(st),tl) = atom in 
							let num_terms = List.length tl in
							Printf.printf " The symbol is %s and has %d number of terms\n" st num_terms ;
							if (num_terms = 0) then () else Printf.printf "  The terms are as follows: \n";(printTerms tl) ;;

(* Used to print a list of atoms . Acts as helper function to printExpression. Made essentially for purposes of testing. *)
let rec printBody body = match body with
			[] -> Printf.printf " Error ... No terms in body\n" ;
			| x::[] -> printAtom x ;
			| x::xs -> printAtom x ; printBody xs ;; 

(* Used to print an expression i.e a head and body pair. Uses the function printBody on both head and body to print the 2 parts. Essentially for testing. *)
let rec printExpression pair = let (head,body) = pair in
								printBody head ;
								printBody body ;;

(* Printing a list of clauses given by variable c . Uses helper function printExpression on each element on clause list. *)
let rec printClauses c= match c with
					[] -> Printf.printf "No clauses scanned\n" ;
					| x::[] -> printExpression x ;
					| x::xs -> printExpression x ; printClauses xs ;;

(* Function used to print a single substitution . This is used to print the substitutions when we have found the answer to the query . 
Since it is possible that variables are introduced during matching of facts but were not a part of the user query . To ditinguish these , we dont
print the substition if the variable of the substituion has   '~' in it , otherwise we print the substitution.*)
let printsubst sb = let Subst(Var(str1),t) = sb in 
						if (String.contains str1 '~') then () else (
						match t with
		   				Node(Symb(str2),[]) -> Printf.printf  "%s : %s" (str1)  str2 ;
						| V(Var(str2)) -> Printf.printf  "%s : %s" (str1)  str2 ;
						| Node(Symb(str2),tl) ->  Printf.printf  "%s : " str1  ; printTerm t);;

(* Function to print a list of substitutions. Uses helper function printsubst . Prints a list of substitutions and acts as helper function to answerQueries. *)
let rec printSubstitutions sbl = match sbl with
							[] -> () ;
							| x::[] -> printsubst x ;
						   | x::xs -> printsubst x ; Printf.printf "\n" ; printSubstitutions xs ;;
						   		

(* Returns the head from the clause. Acts as helper function for answerQueries. *)
let getTerm clause = let (head,body) = clause in (List.hd head) ;;

(* Tweaks a given query such that if it is a dont care i.e  "_" then we replace it with the variable named "DONT_CARE" and 
append the variable num to this string. This allows us to deal with dont cares that might be present in the queries or in the list of facts.
The function returns the tweaked query as well as the number of times a dont care was encountered in the query term . *)
let rec tweakQuery q num = match q with
				| V(Var(str1)) -> if (str1 = "_") then (V(Var("DONT_CARE"^(string_of_int num)^"~")),(num + 1)) else (V(Var(str1)),num) ;
				| Node(sym,tl) -> match tl with
				                  [] -> (Node(sym,[]),num) ;
				                 | t::ts -> let (temp,temp2) = (tweakQuery t num) in 
				                 			let (temp3,temp4) = (tweakQuery (Node(sym,ts)) temp2) in 
				                 			let Node(_,temp5) = temp3 in (Node(sym,temp::temp5),temp4) ;;  
									
(* Function which tweaks a list of queries  by applying the function tweakQuery on each element on the list of queries and returns )the
 tweaked list,the total number of tweaks applied to the list queries) as a tuple.*)
let rec tweakQueries queries num = match queries with
					[] -> ([],num) ;
				  | q::qs -> let (x,y) = tweakQuery q num in
				  				let (a,b) = (tweakQueries qs y) in (x::a,b) ;;


(* This function tweaks a term by appending "~~" to every variable in the term . This is important so that variable used in user query do not 
  coincide with variable in the fact list which will cause problems with the functioning of the program . Acts as helper function to tweakClause and tweakCluse2  *)
let rec tweakTerm term = match term with
				  Node(Symb(str2),tl) -> Node(Symb(str2),(List.map tweakTerm tl)) ;
					| V(Var(str2)) -> V(Var(str2^"~~")) ;;

(* Used to change clause such that dont cares are handled as mentioned in the documentation for the function tweakQuery . We ensure that the dont cares in the head 
are not the same as the dont cares for the body and hence we use the number returned by calling tweakQueries on head , as input in calling tweakQueries on body . *)
let rec tweakClause c = let (head,body) = c in 
							let (head2,num) = (tweakQueries head 1) in
								let (body2,num2) = (tweakQueries body (num + 1)) in
									((List.map tweakTerm head2),(List.map tweakTerm body2)) ;;


(* Function to add "~~" to all terms in clauses by using the function tweakTerm on terms in head as well as terms in body . *)
let rec tweakClause2 c = let (head,body) = c in ((List.map tweakTerm head),(List.map tweakTerm body)) ;;

(* function used to reverse Substitutions such that if substituion is from variable to varbale for example : X---> Y , then it is changed to 
 Y ---> X . This is applied to all the substitution in the list and this new list is returned by the function . This acts as helper function to answerQueries
  and is used when the substition obtained is not for a rule but rather for a fact . Hence rather than applying the substition to a body ,
 we need to print it as an answer where we require that the substituionf os the form X ----> Y are reversed . Hence we reverse it and return this list . *)
let rec reverseSubs sbl = match sbl with
			[] -> []
			| sb::sbs -> let Subst(v,t) = sb in match v,t with
							Var(z),V(Var(y)) -> (Subst(Var(y),V(Var(z))))::(reverseSubs sbs) ;
							| _,_ -> sb::(reverseSubs sbs) ;; 


let shouldContinue (x: unit) = 
	let str_temp = read_line () in
			if(str_temp = ";") then true else false ;;





(* This is the main function in this entire program that takes in a list of queries and clauses and returns the appropriate answers to those queries.
For doing this we use the mgu function implemeted in assignment 5 on the list of queries and match each clause with a single query . 
If the clause and query are unifiable, then we apply this substitution to the rest of the queries and apply the function answerQueries recursively 
on this new list of queries . When no more queries remain , we simply print the obtained substitution list which is a single answer obtained for the query .

NOTE : WE PRINT ALL THE ANSWERS TO THE QUERY AND USER CANNOT CHOSE TO PRINT JUST A SINGLE SUBSTITUION. Yet incorporating this functionality into the program is 
very easy using the existing functions in our program . 

Also , to deal with rules which have both a head and a body , we first apply mgu on head and clause and if they are unifiable , the substituions are
applied to the terms in the body of the rule . Then this new body is appended to the list of queries and function is recursively applied 
on this new list of queries . 

To deal with  queries that might lead to the search space being infinite , we need to incorporate a factor depth using which we only explore the search space
to a prespecified depth as denoted by the paraneter lim_depth . The parameter depth increases by 1 when we move 1 depth lower in the search tree and when the 
depth = lim_depth , we stop the search .

This function returns true if we found one or more answers to the query . Otherwise , false is returned . 
 *)
let rec answerQueries queries clauses subs all_clauses depth lim_depth flag_found= 
								if(depth = lim_depth) then (flag_found,true) else (
								match queries with
								[]->raise(InvalidQuery) ;
							|   q::[] -> 
										(match clauses with
											[] -> (flag_found,true) ;
											| clause::xs ->
													let x = (getTerm clause) in
													let (flag,subst_list) = (mgu x q) in 
															if(flag) then (
																let (head,body) = clause in
																	if((List.length body) = 0) then ( 
																			   (* printSubstitutions subst_list ; *)
																			   printSubstitutions (subs@( reverseSubs subst_list)) ;
																			   
																			   if (shouldContinue ()) then (
																			   		Printf.printf  "\n" ; flush stdout ;
																			  		(answerQueries queries xs subs all_clauses depth lim_depth true)  ;)
																				else (true,false))
																	else (
																			(* Printf.printf  "\n----TESTING-----\n" ; flush stdout ;
																			printBody body ;
																			Printf.printf  "\n----TESTING4-----\n" ; flush stdout ;
																			printSubstitutions subst_list ;
																			Printf.printf  "\n" ; flush stdout ; *)
																		let queries2 = (List.map (subst subst_list) body) in 
																			let all_clauses2 = (List.map tweakClause2 all_clauses) in
																			(* printBody queries2 ; *)
																			let (flag2,flagCont) = (answerQueries queries2 all_clauses2 subs all_clauses2 (depth + 1) lim_depth flag_found) in
																				if(flagCont) then 
																					(let (f1,f2) = (answerQueries queries xs subs all_clauses depth lim_depth flag_found) in ((flag2 || f1),f2);)
																				else (flag2,false) ;
																	)
																
															)
															else (answerQueries queries xs subs all_clauses depth lim_depth flag_found)  ;)

							|   q::qs -> 
										match clauses with
											[] -> (flag_found,true) ;
											| clause::xs -> 
													let x = (getTerm clause) in
													let (flag,subst_list) = (mgu x q) in  
															if(flag) then (
																let (head,body) = clause  and all_clauses2 = (List.map tweakClause2 all_clauses) in
																	if((List.length body) = 0) then (
																				let rev_subs = reverseSubs subst_list in
																			   let subst_queries = (List.map (subst rev_subs) qs) in 
																			   		let new_subs = (subs@rev_subs) in
																					(* Printf.printf  "\n----TESTING4-----\n" ; flush stdout ;
																					printSubstitutions subst_list ;
																			   		Printf.printf  "\n" ; flush stdout ; *)
																			   		(* Printf.printf  "\n----TESTING6-----\n" ; flush stdout ;
																					printSubstitutions new_subs ; *)
																					(* Printf.printf  "\n----TESTING7-----\n" ; flush stdout ;
																					printBody subst_queries ;
																			   		Printf.printf  "\n" ; flush stdout ; *)
																					let (flag2,flagCont) = (answerQueries subst_queries all_clauses2 new_subs all_clauses2 (depth + 1) lim_depth flag_found) in 
																						if(flagCont) then 
																							(let (f1,f2) = (answerQueries queries xs subs all_clauses depth lim_depth flag_found) in ((flag2 || f1),f2);)
																						else (flag2,false) ;)
																	else (
																		 (* Printf.printf  "\n----TESTING2-----\n" ; flush stdout ;
																			printBody body ;
																			printSubstitutions subst_list ;
																			Printf.printf  "\n" ; flush stdout ; *)
																	let queries2 = ((List.map (subst subst_list) body)@qs) in
																		let (flag2,flagCont) = (answerQueries queries2 all_clauses2 subs all_clauses2 (depth + 1) lim_depth flag_found) in 
																				if(flagCont) then 
																							(let (f1,f2) = (answerQueries queries xs subs all_clauses depth lim_depth flag_found) in ((flag2 || f1),f2);)
																							else (flag2,false) ;)
															)
															else (answerQueries queries xs subs all_clauses depth lim_depth flag_found))  ;;
