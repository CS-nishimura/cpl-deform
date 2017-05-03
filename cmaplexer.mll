{

open Proc
open Cmapparser
open Lexing

exception Lexical_error

(* sub functions for parsing double-quotated string *)
(*
let lex_string_buffer = ref ([] : char list)

let reset_string_buffer () = lex_string_buffer := []
let push_to_string_buffer c = lex_string_buffer := c::!lex_string_buffer
let get_string_buffer () =
  let buf = !lex_string_buffer in
  let len = List.length buf in
  let str = String.create len in
  let _ = List.fold_left
      (fun pos c -> str.[pos]<-c; pos-1) (len-1) buf
  in str
*)

}


let newline = ('\013'* '\010')
let blank = [' ' '\t' '\r' '\n']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let arabic = ['0'-'9']

rule token = parse
  blank	{ token lexbuf }	(* skip blanks *)
| "/*"			{ comment lexbuf }	(* beginning of a comment *)
| "->"			{ ARROW }		(* arrow *)
| "=>"			{ DARROW }		(* double arrow *)
| '$'			{ DOLLAR }		(* dollar *)
| '+'			{ PLUS }		(* plus *)
| '-'			{ MINUS }		(* minus *)
| '*'			{ TIMES }		(* times *)
| '%'			{ PERCENT }		(* percent *)
| '|'			{ BAR }			(* bar *)
| ','			{ COMMA }		(* a separator between attributes *)
| '{'			{ LBRACE }		(* left brace for grouping *)
| '}'			{ RBRACE }		(* right brace for grouping *)
| '('			{ LPAREN }		(* left parenthesis *)
| ')'			{ RPAREN }		(* right parenthesis *)
| ";;"			{ DSCOLON }		(* delimiter *)
| "Colors:"		{ COLORS }		(* keyword: color sysmbols declaration *)
| "Cmap:"		{ CMAP }		(* keyword: carrier map declaration *)
| arabic + as num 	{ NUM num }	(* numbers *)
| uppercase identchar * as ident { ALPH ident }	(* identifiers *)
| eof			{ EOF }			(* end of file *)

(* | '"'	{ reset_string_buffer (); STRING(string lexbuf) }

and string = parse
    '"'
			{ get_string_buffer () }
| '\\' eof
			{ raise Lexical_error }
| '\\' _		{ push_to_string_buffer (lexeme_char lexbuf 1);
			  string lexbuf }
|  _			{ push_to_string_buffer (lexeme_char lexbuf 0);
			  string lexbuf }
*)

and comment = parse
  "*/"			{ token lexbuf }	(* end of a comment *)
| _			{ comment lexbuf }	(* in a comment *)
