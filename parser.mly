/* parser.mly */

%token<float> NUMBER
%token<string> STRING NAME COMMENT

%token BRA KET BADTOK

/* Postscript operators */
%token CURVETO LINETO MOVETO SHOWPAGE NEWPATH FSHOW CLOSEPATH FILL
%token STROKE CLIP RLINETO SETLINEJOIN SETLINECAP SETMITERLIMIT
%token SETGRAY SETRGBCOLOR SETDASH GSAVE GRESTORE
%token TRANSLATE SCALE CONCAT DTRANSFORM SETLINEWIDTH POP
%token EXCH TRUNCATE IDTRANSFORM

/* Header comments */
%token BBOX HIRESBB

%type <unit> file
%start file

%{
open Printf

(* litopen -- whether inside literal code *)
let litopen = ref false

let start () =
  if !litopen then printf "\n  " else printf "\\pdfliteral{";
  litopen := true

let stop () =
  if !litopen then printf "}\n";
  litopen := false

(* pdfop -- output PDF operator with numeric arguments *)
let pdfop op args =
  start ();
  List.iter (fun x -> printf "%g " x) args;
  printf "%s" op

(* setdash -- output setdash (d) operator *)
let setdash xs y =
  start ();
  printf "[ ";
  List.iter (fun x -> printf "%g " x) xs;
  printf "] %g d" y

(* concat -- output concat (cm) operator *)
let concat (a, b, c, d, e, f) =
  pdfop "cm" [a; b; c; d; e; f]

(* matrix -- form matrix from list of values *)
let matrix =
  function
      [a; b; c; d; e; f] -> (a, b, c, d, e, f)
    | _ -> failwith "matrix"

(* invert -- inverse of a transformation matrix *)
let invert (a, b, c, d, e, f) =
  let det = a *. d -. b *. c in
  (d /. det, -. b /. det, -. c /. det, a /. det, 
    (c *. f -. d *. e) /. det, (b *. e -. a *. f) /. det) (* Phew! *)

(* apply -- apply transformation to a vector *)
let apply (a, b, c, d, e, f) (x, y) = 
  (a *. x +. c *. y +. e, b *. x +. d *. y +. f)

(* dapply -- apply transformation to a displacement *)
let dapply (a, b, c, d, e, f) (dx, dy) = 
  (a *. dx +. c *. dy, b *. dx +. d *. dy)

(* element -- type of path elements *)
type element =
    MoveTo of float * float
  | LineTo of float * float
  | RLineTo of float * float
  | CurveTo of float * float * float * float * float * float
  | ClosePath

(* trpath -- apply transformation to a path *)
let trpath t p = 
  let tr =
    function
	MoveTo (x, y) -> 
	  let (u, v) = apply t (x, y) in MoveTo (u, v)
      | LineTo (x, y) -> 
	  let (u, v) = apply t (x, y) in LineTo (u, v)
      | RLineTo (dx, dy) -> 
	  (* MetaPost always has dx = dy = 0, but let's do the right thing. *)
	  let (du, dv) = dapply t (dx, dy) in RLineTo (du, dv)
      | CurveTo (x1, y1, x2, y2, x3, y3) ->
	  let (u1, v1) = apply t (x1, y1) in
	  let (u2, v2) = apply t (x2, y2) in
	  let (u3, v3) = apply t (x3, y3) in
	  CurveTo (u1, v1, u2, v2, u3, v3) 
      | ClosePath -> ClosePath in
  List.map tr p

(* path -- output a path *)
let path op p =
  (* Keep track of the current point so RLineTo can turn into LineTo *)
  let pr (x0, y0) =
    function
	MoveTo (x, y) -> pdfop "m" [x; y]; (x, y)
      | LineTo (x, y) -> pdfop "l" [x; y]; (x, y)
      | RLineTo (dx, dy) -> 
	  let x = x0 +. dy and y = y0 +. dy in
	  pdfop "l" [x; y]; (x, y)
      | CurveTo (x1, y1, x2, y2, x3, y3) ->
	  pdfop "c" [x1; y1; x2; y2; x3; y3]; (x3, y3)
      | ClosePath -> pdfop "h" []; (x0, y0) in
  ignore (List.fold_left pr (0.0, 0.0) p);
  pdfop op []

(* transpath -- output a path and establish pen transformation *)
let transpath op t p =
  (* Postscript lets us put the transformation after the path, and uses the
     old matrix for the path elements.  PDF is different, so we have to
     compensate by applying the inverse transformation to the path. *)
  concat t;
  path op (trpath (invert t) p)

(* currx, curry -- coordinates from last moveto *)
let currx = ref 0.0
let curry = ref 0.0

(* moveto -- save data from moveto not in a path *)
let moveto x y =
  currx := x; curry := y

(* safe -- test if character is safe for TeX *)
let safe c =
  c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9'

(* The text in an fshow must pass through TeX, or the characters that
   are used will not be seen by pdfTeX's font subset machinery.  But we
   need to be careful that TeX doesn't 'helpfully' insert ligs and kerns,
   so we \relax between each character and the next. *)

(* fshow -- output macro call for fshow operator *)
let fshow str fnt siz =
  stop ();
  printf "\\fshow{%g}{%g}{" !currx !curry;
  for i = 0 to String.length str - 1 do
    let c = str.[i] in
    if safe c then
      printf "%c\\\\" c
    else
      printf "\\char'%03o\\\\" (int_of_char c)
  done;
  printf "}{%s}{%g}\n" fnt siz

(* bblevel, llx, etc. -- data about bounding box *)
let bblevel = ref 0
let llx = ref 0.0
let lly = ref 0.0
let urx = ref 0.0
let ury = ref 0.0

(* bbox -- save most accurate bounding box info *)
let bbox level x0 y0 x1 y1 =
  if level > !bblevel then begin
    llx := x0; lly := y0;
    urx := x1; ury := y1;
    bblevel := level
  end

(* header -- start the output *)
let header () =
  List.iter (fun s -> printf "%s\n" s)
    [ "\\begingroup";
      "\\def\\fshow#1#2#3#4#5{%";
      "  {\\setbox1=\\hbox{\\hskip#1bp \\raise#2bp";
      "      \\hbox{\\font\\f=#4 at#5bp \\let\\\\=\\relax \\f#3}}%";
      "    \\ht1=0pt \\dp1=0pt \\wd1=0pt \\box1}}%";
      "\\setbox0=\\vbox{\\offinterlineskip" ];
  (* Initial origin is in the top left corner of the bounding box *)
  pdfop "q" [];
  concat (1.0, 0.0, 0.0, 1.0, -. !llx, -. !ury)

(* trailer -- finish the output *)
let trailer () =
  pdfop "Q" [];
  stop ();
  printf "}%%\n";
  printf "\\wd0=%gbp \\ht0=%gbp \\box0\n" (!urx -. !llx) (!ury -. !lly);
  printf "\\endgroup\n"
%}

%%

file :
    preamble body show			{ () } ;

preamble :
    /* empty */				{ () }
  | preamble comment			{ () } ;

comment :
    BBOX NUMBER NUMBER NUMBER NUMBER	{ bbox 1 $2 $3 $4 $5 }
  | HIRESBB NUMBER NUMBER NUMBER NUMBER	{ bbox 2 $2 $3 $4 $5 }
  | COMMENT				{ printf "%s\n" $1 } ;

body :
    /* empty */				{ header () }
  | body command			{ () } ;

show :
    SHOWPAGE				{ trailer () } ;

command :
    NUMBER NUMBER MOVETO		{ moveto $1 $2 }
  | GSAVE				{ pdfop "q" [] }
  | GRESTORE				{ pdfop "Q" [] }
  | path CLIP				{ path "W n" $1 }
  | path STROKE				{ path "S" $1 }
  | /* drawing with an elliptical pen: */
    path transform STROKE		{ transpath "S" $2 $1 }
  | path FILL				{ path "f" $1 }
  | /* filldraw (round pen): */
    path GSAVE FILL GRESTORE STROKE	{ path "B" $1 }
  | /* filldraw (elliptical pen): */
    path GSAVE FILL GRESTORE transform STROKE  
					{ transpath "B" $5 $1 }
  | STRING NAME NUMBER FSHOW		{ fshow $1 $2 $3 }
  | /* These are versions of setlinewidth with fancy device-dependent
       rounding.  We just use the simple setlinewidth (w): */
    NUMBER NUMBER DTRANSFORM EXCH TRUNCATE EXCH IDTRANSFORM POP SETLINEWIDTH
					{ pdfop "w" [$1] }
  | NUMBER NUMBER DTRANSFORM TRUNCATE IDTRANSFORM SETLINEWIDTH POP
					{ pdfop "w" [$2] }
  | BRA numbers KET NUMBER SETDASH	{ setdash $2 $4 }
  | NUMBER NUMBER NUMBER SETRGBCOLOR	{ pdfop "rg" [$1; $2; $3];
					  pdfop "RG" [$1; $2; $3] }
  | NUMBER SETGRAY			{ pdfop "g" [$1]; pdfop "G" [$1] }
  | NUMBER SETLINEJOIN			{ pdfop "j" [$1] }
  | NUMBER SETMITERLIMIT		{ pdfop "M" [$1] }
  | NUMBER SETLINECAP			{ pdfop "J" [$1] }
  | transform				{ concat $1 }

transform :
    BRA numbers KET CONCAT 		{ matrix $2 }
  | NUMBER NUMBER TRANSLATE		{ (1.0, 0.0, 0.0, 1.0, $1, $2) }
  | NUMBER NUMBER SCALE			{ ($1, 0.0, 0.0, $2, 0.0, 0.0) } ;

path :
    NEWPATH				{ [] }
  | path pathseg			{ $1 @ [$2] } ;

pathseg :
    NUMBER NUMBER MOVETO		{ MoveTo ($1, $2) }
  | NUMBER NUMBER LINETO		{ LineTo ($1, $2) }
  | NUMBER NUMBER RLINETO		{ RLineTo ($1, $2) }
  | NUMBER NUMBER NUMBER NUMBER NUMBER NUMBER CURVETO
					{ CurveTo ($1, $2, $3, $4, $5, $6) }
  | CLOSEPATH				{ ClosePath }

numbers :
    /* empty */				{ [] }
  | NUMBER numbers			{ $1 :: $2 } ;
