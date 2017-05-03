%{

open Locat
open Valexp

let mkabs x =
  start_loc := Parsing.symbol_start ();
  end_loc := Parsing.symbol_end ();
  x

let expvalmap procs e =
  List.map
    (fun {Proc.color=color;Proc.value=value} ->
      Proc.mkproc color (interp ~value e)) procs

%}
%token <string> ALPH NUM
%token COLORS CMAP
%token COMMA
%token DSCOLON
%token BAR
%token LBRACE RBRACE LPAREN RPAREN
%token ARROW DARROW
%token DOLLAR PLUS MINUS TIMES PERCENT
%token EOF


/* precedence */
%left BAR
%right COMMA
%left PLUS MINUS
%left TIMES PERCENT
%nonassoc prec_uminus

%start main
%type <Proc.color list * Proc.concrete_cmap> main
%%
main:
  COLORS lcmapdecl 		{ mkabs ([], $2) }  /* colorless */
| COLORS colors cmapdecl 	{ mkabs ($2, $3) }  /* chromaic */

/* color list */
colors:
  color				{ mkabs [$1] }
| color colors			{ mkabs ($1::$2) }

color:
  ALPH				{ mkabs $1 }

/* expressions in spec */
exp:
  NUM  				{ mkabs (Enum (int_of_string $1)) }
| MINUS exp			{ mkabs (Euminus $2) } %prec prec_uminus
| exp PLUS exp			{ mkabs (Ebin(Oplus,$1,$3)) }
| exp MINUS exp			{ mkabs (Ebin(Ominus,$1,$3)) }
| exp TIMES exp			{ mkabs (Ebin(Omult,$1,$3)) }
| exp PERCENT exp		{ mkabs (Ebin(Omod,$1,$3)) }
| LPAREN exp RPAREN		{ mkabs $2 }

expval:
  NUM  				{ mkabs (Enum (int_of_string $1)) }
| MINUS expval			{ mkabs (Euminus $2) } %prec prec_uminus
| expval PLUS expval		{ mkabs (Ebin(Oplus,$1,$3)) }
| expval MINUS expval		{ mkabs (Ebin(Ominus,$1,$3)) }
| expval TIMES expval		{ mkabs (Ebin(Omult,$1,$3)) }
| expval PERCENT expval		{ mkabs (Ebin(Omod,$1,$3)) }
| DOLLAR			{ mkabs Evalue }
| LPAREN expval RPAREN		{ mkabs $2 }

/* delimited */
delim:
  DSCOLON		{ mkabs () }

maydelim:
     			{ mkabs () }
| delim			{ mkabs () }



/* chromatic delarations */
cmapdecl:
  CMAP cmaps  			{ mkabs $2 }

cmaps:
  cmap  			{ mkabs [$1] }
| cmap cmaps			{ mkabs ($1::$2) }

cmap:
  procs ARROW mapout maydelim	{ mkabs ($1, $3) }
| procs DARROW expval maydelim	{ mkabs ($1, [expvalmap $1 $3]) }

procs:
  proc  			{ mkabs [$1] }
| proc COMMA procs		{ mkabs ($1::$3) }

proc:
  color exp	  		{ mkabs (Proc.mkproc $1 (interp ~value:0 $2)) }

mapout:
  LBRACE gprocs RBRACE 		{ mkabs $2 }

gprocs:
  procs  			{ mkabs [$1] }
| procs BAR gprocs		{ mkabs ($1::$3) }


/* colorless delarations */
lcmapdecl:
  CMAP lcmaps  			{ mkabs $2 }

lcmaps:
  lcmap  			{ mkabs [$1] }
| lcmap lcmaps			{ mkabs ($1::$2) }

lcmap:
  lprocs ARROW lmapout maydelim	{ mkabs ($1, $3) }
| lprocs DARROW expval delim	{ mkabs ($1, [expvalmap $1 $3]) }

lprocs:
  lproc  			{ mkabs [$1] }
| lproc COMMA lprocs		{ mkabs ($1::$3) }

lproc:
  exp	  			{ mkabs (Proc.mkproc "" (interp ~value:0 $1)) }

lmapout:
  LBRACE lgprocs RBRACE 	{ mkabs $2 }

lgprocs:
  lprocs  			{ mkabs [$1] }
| lprocs BAR lgprocs		{ mkabs ($1::$3) }
