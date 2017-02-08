
%{ (* A parser for Jape's Hoare logic encoding *) 
		open Parsetree
%} /* declarations */

/* tokens */
%token  GAMMA
%token  COMMA
%token  STILE
%token  BRA
%token  KET
%token  SQBRA
%token  SQKET
%token  CURLYBRA
%token  CURLYKET
%token  SUBSTBRA
%token  SUBSTSEP
%token  SUBSTKET
%token  BOTTOM
%token  TOP
%token  BECOMES
%token  SEMICOLON
%token  EXCEPTAT
%token  MAPSTO
%token  IMPLIES 
%token  IFF
%token  NOT
%token  OR
%token  AND
%token  <Parsetree.binder> BIND
%token  DOT
%token  <Parsetree.comparator> COMPARE
%token  PLUS
%token  MINUS
%token  TIMES
%token  DIV
%token  MOD
%token  EXP
%token  INTEGER
%token  IFBRA
%token  THENSEP
%token  ELSESEP
%token  FIKET
%token  WHILEBRA
%token  DOSEP
%token  ODKET
%token  SKIP
%token  TILT
%token <string> NUMBER
%token <string> IDENT
%token EOL

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

/* How I hate Knuth-type parsing -- how on earth do we make juxtaposition left-associative? 
   Answer: don't use these stupid precedences, write a proper grammar as below
 */
%nonassoc STILE
%right COMMA
%nonassoc INTEGER
%left SEMICOLON
%nonassoc BECOMES
%left EXCEPTAT MAPSTO
%right IMPLIES IFF
%left AND
%left OR
%right BIND DOT
%nonassoc COMPARE
%left PLUS MINUS
%left TIMES DIV MOD
%right EXP
%right NOT
%left belowBRA
%left SUBSTBRA SUBSTKET
%nonassoc IFBRA WHILEBRA BRA SQBRA CURLYBRA BOTTOM TOP IDENT NUMBER SKIP TILT

/* entry points */

%start sequent
%type <Parsetree.sequent> sequent

%start formula
%type <Parsetree.formula> formula

%% /* rules */

/* entry points */

sequent:
  | context STILE formula EOL
			{ Sequent($1, $3) }
	| formula EOL
			{ Sequent([],$1) }

context:
	| antecedent 
			{ [$1] }
	| antecedent COMMA context
			{ $1 :: $3 }

antecedent:
	| GAMMA
			{ Gamma }
	| INTEGER name
			{	Integer $2 }
	| formula
	    { Assert $1 }
			
name:
	| IDENT
			{ Var $1 }
			
primary:
	| name                  
			{ Ident $1 }
	| NUMBER
			{ Number $1 }
	| BOTTOM
			{ Bottom }
	| TOP
			{ Top }
	| SKIP
			{ Skip }
	| TILT
			{ Tilt }
	| BRA formulalist KET           
			{ Bra($2) }
	| SQBRA nonemptyformulalist SQKET           
			{ Subscript($2) }
	| CURLYBRA formula CURLYKET           
			{ PrePost($2) }
	| IFBRA formula THENSEP formula ELSESEP formula FIKET
			{ Choice($2, $4, $6) }
	| WHILEBRA formula DOSEP formula ODKET
			{ Repetition($2, $4) }

secondary:
	| primary { $1 }
	| secondary	SUBSTBRA namelist SUBSTSEP formulalist SUBSTKET 
			{ Substitution($1, $3, $5) }

tertiary:
	| secondary { $1 }
	| tertiary secondary %prec belowBRA          
			{ Juxtaposition($1, $2) }

formula:
	| tertiary { $1 }	
	| formula SEMICOLON formula 
			{ Sequence($1, $3) }
	| formula BECOMES formula   
			{ Assign($1, $3) }
			
	| formula EXCEPTAT formula MAPSTO formula    
			{ Except($1, $3, $5) }
			
	| formula IMPLIES formula   
			{ InfixLogic(Implies, $1, $3) }
	| formula IFF formula       
			{ InfixLogic(Iff, $1, $3) }
	| formula OR formula        
			{ InfixLogic(Or, $1, $3) }
	| formula AND formula       
			{ InfixLogic(And, $1, $3) }
			
	| BIND name DOT formula      
			{ Bind($1, $2, $4) }
			
	| formula COMPARE formula    
			{ Compare($2, $1, $3) }
			
	| formula PLUS formula      
			{ InfixArith(Plus, $1, $3) }
	| formula MINUS formula      
			{ InfixArith(Minus, $1, $3) }
	| formula TIMES formula     
			{ InfixArith(Times, $1, $3) }
	| formula DIV formula     
			{ InfixArith(Div, $1, $3) }
	| formula MOD formula     
			{ InfixArith(Mod, $1, $3) }
	| formula EXP formula       
			{ InfixArith(Exp, $1, $3) }
			
	| NOT formula               
			{ Not($2) }
					
formulalist:
		/* empty */
			{ [] }
	| nonemptyformulalist
			{ $1 }

nonemptyformulalist:
	| formula { [$1] }
	| formula COMMA nonemptyformulalist { $1 :: $3 }

namelist:
		/* empty */
			{ [] }
	| nonemptynamelist
			{ $1 }

nonemptynamelist:
  | name
			{ [$1] }
	|  name COMMA nonemptynamelist
			{ $1 :: $3 }
	
%% (* trailer *)
