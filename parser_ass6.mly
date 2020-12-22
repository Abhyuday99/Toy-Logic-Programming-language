%{

open Ass6_backend
	%}
%token ASSIGN LEFCL RGTCL COMMA TERM CONTINUE
%token <string> CONSTANT
%token <string> VARIABLE
%start main             /* the entry point */
%type <Ass6_backend.clause> main
%%



main:
	expr TERM		 {$1}
	| error			{raise InvalidInput} ;

expr:
	body ASSIGN body { ($1,$3) }
	| body         { ($1,[])} ;

atomic_form:
	CONSTANT LEFCL term_list RGTCL {Node(Symb($1),$3)}
	| CONSTANT {Node(Symb($1),[])} ;

term_list:
	term {[$1]}
	| term COMMA term_list {$1 :: $3} ;

term:
	CONSTANT {Node(Symb($1),[])}
	| VARIABLE {V(Var($1))}
	| CONSTANT LEFCL term_list RGTCL {Node(Symb($1),$3)} ;

body:
	atomic_form {[$1]}
	| atomic_form COMMA body { $1 :: $3} ;
