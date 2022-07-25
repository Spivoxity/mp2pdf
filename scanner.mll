(* scanner.mll *)

{
open Parser

let optable = Hashtbl.create 103

let init =
  let add (s, v) = Hashtbl.add optable s v in
  List.iter add
    ["curveto", CURVETO;
      "lineto", LINETO;
      "moveto", MOVETO;
      "showpage", SHOWPAGE;
      "newpath", NEWPATH;
      "fshow", FSHOW;
      "closepath", CLOSEPATH;
      "fill", FILL;
      "stroke", STROKE;
      "clip", CLIP;
      "rlineto", RLINETO;
      "setlinejoin", SETLINEJOIN;
      "setlinecap", SETLINECAP;
      "setmiterlimit", SETMITERLIMIT;
      "setgray", SETGRAY;
      "setrgbcolor", SETRGBCOLOR;
      "setdash", SETDASH;
      "gsave", GSAVE;
      "grestore", GRESTORE;
      "translate", TRANSLATE;
      "scale", SCALE;
      "concat", CONCAT;
      "dtransform", DTRANSFORM;
      "setlinewidth", SETLINEWIDTH;
      "pop", POP;
      "truncate", TRUNCATE;
      "exch", EXCH;
      "idtransform", IDTRANSFORM]

let lookup s =
  try Hashtbl.find optable s with Not_found -> NAME s

let strbuf = Buffer.create 128
}

rule token = parse
    ['A'-'Z''a'-'z']['A'-'Z''a'-'z''0'-'9']+ as s	
				{ lookup s }
  | '-'?['0'-'9']+('.'['0'-'9']+)? as s
				{ NUMBER (float_of_string s) }
  | '('				{ Buffer.clear strbuf; STRING (string lexbuf) }
  | '['				{ BRA }
  | ']'				{ KET }
  | '%'				{ skip lexbuf; token lexbuf }
  | "%%BoundingBox:"		{ BBOX }
  | "%%HiResBoundingBox:" 	{ HIRESBB }
  | (("%%Creator:"|"%%CreationDate:")[^'\n']* as s)'\n'
				{ COMMENT s }
  | [' ''\n']			{ token lexbuf }
  | _				{ BADTOK }

and string = parse
    [^')'] as c			{ Buffer.add_char strbuf c; string lexbuf }
  | '\\'([^'0'-'7'] as c)  	{ Buffer.add_char strbuf c; string lexbuf }
  | '\\'(['0'-'7']['0'-'7']['0'-'7'] as s)
				{ Buffer.add_char strbuf 
				    (char_of_int (int_of_string ("0o" ^ s)));
				  string lexbuf }
  | ')' 			{ Buffer.contents strbuf }

and skip = parse
    [^'\n']*'\n'		{ () }

{
open Lexing
open Printf

let main =
  if Array.length Sys.argv <> 2 then begin
    fprintf stderr "Usage: mp2pdf file.1\n";
    exit 2
  end;
  let fname = Sys.argv.(1) in
  let fp = 
    try open_in fname with Sys_error _ ->
      fprintf stderr "mp2pdf: couldn't read %s\n" fname; exit 1 in
  printf "%% Converted by mp2pdf from %s\n" fname;
  let lexbuf = Lexing.from_channel fp in
  try
    Parser.file token lexbuf
  with Parsing.Parse_error ->
    fprintf stderr "syntax_error at '%s'\n" (lexeme lexbuf)
}
