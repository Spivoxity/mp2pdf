/* parser.mly */
/* Copyright (c) 2021 J. M. Spivey */

%{
#define YYDEBUG 1
%}

%union {
     double d;
     char *s;
     int len;     
}

%start file

%token<d> NUMBER
%token<s> NAME COMMENT
%token<len> STRING /* Characters are in yystring, can include '\0'. */

%token BRA KET BADTOK

/* Postscript operators */
%token CURVETO LINETO MOVETO SHOWPAGE NEWPATH FSHOW CLOSEPATH FILL
%token STROKE CLIP RLINETO SETLINEJOIN SETLINECAP SETMITERLIMIT
%token SETGRAY SETRGBCOLOR SETDASH GSAVE GRESTORE
%token TRANSLATE SCALE CONCAT DTRANSFORM SETLINEWIDTH POP
%token EXCH TRUNCATE IDTRANSFORM

/* Header comments */
%token BBOX HIRESBB

%{
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX 1024

extern char yystring[];

int yylex(void);

void yyerror(char *msg) {
     fprintf(stderr, "%s\n", msg);
}

void panic(char *msg) {
     fprintf(stderr, "mp2pdf: panic -- %s\n", msg);
     exit(2);
}

/* litopen -- whether inside literal code */
int litopen = 0;

/* stack -- list of values for setdash and concat */
double stack[MAX];
int nstack = 0;

static int pos = 0;

void putword(char *s) {
     int len = strlen(s);

     if (pos + 1 + len > 72) {
          fputs("\n  ", stdout); pos = 2;
     } else if (pos > 0) {
          putchar(' '); pos++;
     }

     fputs(s, stdout); pos += len;
}

void start(void) {
     if (!litopen) putword("\\pdfliteral{");
     litopen = 1;
}

void stop(void) {
     if (litopen) fputs(" }\n", stdout);
     litopen = 0; pos = 0;
}

void putnum(double x) {
     char buf[32];

     if (x == (int) x)
	  sprintf(buf, "%d", (int) x);
     else {
	  int i;
	  sprintf(buf, "%.5f", x);
	  i = strlen(buf)-1;
	  while (buf[i] == '0') buf[i--] = '\0';
     }

     putword(buf);
}

#define pdfop(args...) putpdf(COUNT(args), args)
#define COUNT(args...) COUNT1(args, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define COUNT1(x1, x2, x3, x4, x5, x6, x7, x8, x9, n, rest...) n

void putpdf(int n, ...) {
     int i;
     va_list va;

     start();
     va_start(va, n);
     for (i = 0; i < n-1; i++)
	  putnum(va_arg(va, double));
     putword(va_arg(va, char *));
     va_end(va);
}

/* setdash -- output setdash (d) operator */
void setdash(double y) {
     int i;

     start();
     putword("[");
     for (i = 0; i < nstack; i++) putnum(stack[i]);
     putword("]");
     putnum(y); putword("d");
}

struct transform {
     double xx, yx, xy, yy, x, y;
} trans;

/* concat -- output concat (cm) operator */
void concat(void) {
     int i;
     start();
     putnum(trans.xx); putnum(trans.yx);
     putnum(trans.xy); putnum(trans.yy);
     putnum(trans.x);  putnum(trans.y); putword("cm");
}

/* transform -- form matrix from six numbers */
void transform() {
     if (nstack != 6) panic("bad matrix");
     trans.xx = stack[0]; trans.yx = stack[1];
     trans.xy = stack[2]; trans.yy = stack[3];
     trans.x = stack[4]; trans.y = stack[5];
}

/* translate -- form translation matrix */
void translate(double dx, double dy) {
     trans.xx = 1.0; trans.yx = 0.0; 
     trans.xy = 0.0; trans.yy = 1.0;
     trans.x = dx; trans.y = dy;
}

/* scale -- form scaling matrix */
void scale(double sx, double sy) {
     trans.xx = sx; trans.yx = 0.0; 
     trans.xy = 0.0; trans.yy = sy;
     trans.x = 0.0; trans.y = 0.0;
}

/* invert -- find inverse of transformation matrix */
void invert() {
     struct transform u;
     double det = trans.xx * trans.yy - trans.yx * trans.xy;
     u.xx = trans.yy / det;
     u.yx = - trans.yx / det;
     u.xy = - trans.xy / det;
     u.yy = trans.xx / det;
     u.x = (trans.xy * trans.y - trans.yy * trans.x) / det;
     u.y = (trans.yx * trans.x - trans.xx * trans.y) / det;
     trans = u;
}

/* apply -- apply transformation to a vector */
void apply(double vec[]) {
     double x = vec[0], y = vec[1];
     vec[0] = trans.xx * x + trans.xy * y + trans.x;
     vec[1] = trans.yx * x + trans.yy * y + trans.y;
}

/* dapply -- apply transformation to a displacement */
void dapply(double vec[]) {
     double dx = vec[0], dy = vec[1];
     vec[0] = trans.xx * dx + trans.xy * dy;
     vec[1] = trans.yx * dx + trans.yy * dy;
}

/* types of path elements */
#define MoveTo 1
#define LineTo 2
#define RLineTo 3
#define CurveTo 4
#define ClosePath 5

/* pathcode, point, npath -- current path */
int pathcode[MAX];
double point[MAX][2];
int npath = 0;

/* append -- add to the path */
void append(int code, double x, double y) {
     if (npath >= MAX) panic("too many path segments");
     pathcode[npath] = code;
     point[npath][0] = x; point[npath][1] = y;
     npath++;
}

/* path -- output the path */
void path(char *op) {
     /* Keep track of the current point so RLineTo can turn into LineTo */
     double x0 = 0.0, y0 = 0.0;
     int i;

     for (i = 0; i < npath; ) {
	  switch (pathcode[i]) {
	  case MoveTo:
	       pdfop(x0 = point[i][0], y0 = point[i][1], "m");
               i++; break;
	  case LineTo:
	       pdfop(x0 = point[i][0], y0 = point[i][1], "l");
	       i++; break;
	  case RLineTo:
	       pdfop(x0 += point[i][0], y0 += point[i][1], "l");
	       i++; break;
	  case CurveTo:
	       pdfop(point[i][0], point[i][1], 
                     point[i+1][0], point[i+1][1], 
                     x0 = point[i+2][0], y0 = point[i+2][1], "c");
	       i += 3; break;
	  case ClosePath:
	       pdfop("h");
	       i++; break;
	  default:
	       panic("path");
	  }
     }

     pdfop(op);
}

/* transpath -- output the path and establish pen transformation */
void transpath(char *op) {
     int i;

     /* Postscript lets us put the transformation after the path, and uses the
	old matrix for the path elements.  PDF is different, so we have to
	compensate by applying the inverse transformation to the path. */
     concat();
     invert();

     for (i = 0; i < npath; ) {
	  switch (pathcode[i]) {
	  case MoveTo: 
	  case LineTo:
	       apply(point[i++]);
	       break;
	  case RLineTo:
	       /* MetaPost always has dx = dy = 0, but 
		  let's do the right thing anyway. */
	       dapply(point[i++]);
	       break;
	  case CurveTo:
	       apply(point[i++]);
	       apply(point[i++]);
	       apply(point[i++]);
	       break;
	  case ClosePath:
	       i++;
	       break;
	  default:
	       panic("transpath");
	  }
     }

     path(op);
}

/* currx, curry -- coordinates from last moveto */
double currx = 0.0, curry = 0.0;

/* moveto -- save data from moveto not in a path */
void moveto(double x, double y) {
     currx = x; curry = y;
}

/* fshow -- output macro call for fshow operator */
void fshow(char *str, int len, char *fnt, double siz) {
     int i;

     /* The text must pass through the TeX typesetting process so that
	the glyphs that are used can be noticed by the font subsetting
	machinery.  But we don't want TeX to insert ligs and kerns, so
	we separate each character from the next with \\, a synonym
	for \relax. */

     stop();
     printf("\\fshow{%.5f}{%.5f}{", currx, curry);
     for (i = 0; i < len; i++) {
	  if (isalnum(str[i]))
	       printf("%c\\\\", str[i]);
	  else
	       printf("\\char'%03o\\\\", (unsigned char) str[i]);
     }
     printf("}{%s}{%.5f}\n", fnt, siz);
}

/* bblevel, llx, etc. -- data about bounding box */
int bblevel = 0;
double llx = 0.0, lly = 0.0, urx = 0.0, ury = 0.0;

/* bbox -- save most accurate bounding box info */
void bbox(int level, double x0, double y0, double x1, double y1) {
     if (level > bblevel) {
	  llx = x0; lly = y0;
	  urx = x1; ury = y1;
	  bblevel = level;
     }
}

/* If \mpsetsize is defined as a macro, then it is called in order to
   calculate the desired size of the image.  On entry, \dimen0 and
   \dimen1 are set to the natural width and height of the image.  The
   macro should reset these to the desired width and height, and also
   define the macro \mpscale to be the required scale factor.  This
   allows us to get exactly the desired size for the image box even if
   the scale factor suffers from a little rounding error.  If
   \mpsetsize is not defined, then we use the existing value of
   \mpscale (if any) to scale the image, and allow the scale to
   default to 1.  The file mp2pdf.tex contains an appropriate
   definition of \mpsetsize. */

char *prelude[] = {
     "\\ifx\\mpsetsize\\undefined",
     "  \\ifx\\mpscale\\undefined \\def\\mpscale{1}\\fi",
     "  \\dimen0=\\mpscale\\dimen0 \\dimen1=\\mpscale\\dimen1",
     "\\else\\mpsetsize\\fi",
     "\\setbox0=\\vbox{\\offinterlineskip",
     "\\def\\fshow#1#2#3#4#5{",
     "  {\\setbox1=\\hbox{\\hskip#1bp \\raise#2bp",
     "      \\hbox{\\font\\f=#4 at#5bp \\let\\\\=\\relax \\f#3}}",
     "    \\ht1=0pt \\dp1=0pt \\wd1=0pt \\box1}}%",
     NULL
};

char *postlude[] = {
     "}%",
     "\\wd0=\\dimen0 \\ht0=\\dimen1 \\box0",
     "\\endgroup",
     NULL
};

/* header -- start the output */
void header(void) {
     printf("\\begingroup\n");
     printf("\\dimen0=%.5fbp \\dimen1=%.5fbp\n", urx-llx, ury-lly);
     for (char **s = prelude; *s != NULL; s++) printf("%s\n", *s);
     pdfop("q");
     putword("\\mpscale\\space"); putword("0"); putword("0");
     putword("\\mpscale\\space"); putword("0"); putword("0"); putword("cm");
     /* Initial origin is in the top left corner of the bounding box */
     translate(-llx, -ury);
     concat();
}

/* trailer -- finish the output */
void trailer(void) {
     pdfop("Q");
     stop();
     for (char **s = postlude; *s != NULL; s++) printf("%s\n", *s);
}

%}

%%

file :
    preamble body show ;

preamble :
    /* empty */				
  | preamble comment ;

comment :
    BBOX NUMBER NUMBER NUMBER NUMBER	{ bbox(1, $2, $3, $4, $5); }
  | HIRESBB NUMBER NUMBER NUMBER NUMBER	{ bbox(2, $2, $3, $4, $5); }
  | COMMENT				{ printf("%s\n", $1); } ;

body :
    /* empty */				{ header(); }
  | body command ;

show :
    SHOWPAGE				{ trailer(); } ;

command :
    NUMBER NUMBER MOVETO		{ moveto($1, $2); }
  | GSAVE				{ pdfop("q"); }
  | GRESTORE				{ pdfop("Q"); }
  | path CLIP				{ path("W n"); }
  | path STROKE				{ path("S"); }
  | /* drawing with an elliptical pen: */
    path transform STROKE		{ transpath("S"); }
  | path FILL				{ path("f"); }
  | /* filldraw (round pen): */
    path GSAVE FILL GRESTORE STROKE	{ path("B"); }
  | /* filldraw (elliptical pen): */
    path GSAVE FILL GRESTORE transform STROKE  { transpath("B"); }
  | STRING NAME NUMBER FSHOW		{ fshow(yystring, $1, $2, $3); }
  | /* These are versions of setlinewidth with fancy device-dependent
       rounding.  We just use the simple setlinewidth (w): */
    NUMBER NUMBER DTRANSFORM EXCH TRUNCATE EXCH IDTRANSFORM POP SETLINEWIDTH
					{ pdfop($1, "w"); }
  | NUMBER NUMBER DTRANSFORM TRUNCATE IDTRANSFORM SETLINEWIDTH POP
					{ pdfop($2, "w"); }
  | BRA numbers KET NUMBER SETDASH	{ setdash($4); }
  | NUMBER NUMBER NUMBER SETRGBCOLOR	{ pdfop($1, $2, $3, "rg");
					  pdfop($1, $2, $3, "RG"); }
  | NUMBER SETGRAY			{ pdfop($1, "g"); pdfop($1, "G"); }
  | NUMBER SETLINEJOIN			{ pdfop($1, "j"); }
  | NUMBER SETMITERLIMIT		{ pdfop($1, "M"); }
  | NUMBER SETLINECAP			{ pdfop($1, "J"); }
  | transform				{ concat(); } ;

transform :
    BRA numbers KET CONCAT		{ transform(); }
  | NUMBER NUMBER TRANSLATE		{ translate($1, $2); }
  | NUMBER NUMBER SCALE			{ scale($1, $2); } ;

path :
    NEWPATH				{ npath = 0; }
  | path pathseg ;

pathseg :
    CLOSEPATH				{ append(ClosePath, 0.0, 0.0); }
  | NUMBER NUMBER MOVETO		{ append(MoveTo, $1, $2); }
  | NUMBER NUMBER LINETO		{ append(LineTo, $1, $2); }
  | NUMBER NUMBER RLINETO		{ append(RLineTo, $1, $2); }
  | NUMBER NUMBER NUMBER NUMBER NUMBER NUMBER CURVETO
      { append(CurveTo, $1, $2); append(0, $3, $4); append(0, $5, $6); } ;

numbers :
    /* empty */				{ nstack = 0; }
  | numbers NUMBER			{ if (nstack >= MAX)
	    				    panic("stack overflow");
					  stack[nstack++] = $2; } ;

%%

extern FILE *yyin;

int main(int argc, char **argv) {
     FILE *fp;

     /* yydebug = 1; */

     if (argc != 2) {
	  fprintf(stderr, "Usage: mp2pdf file.1\n");
	  exit(2);
     }

     fp = fopen(argv[1], "r");
     if (fp == NULL) {
	  fprintf(stderr, "mp2pdf: couldn't read %s\n", argv[1]);
	  exit(1);
     }

     printf("%% Converted by mp2pdf from %s\n", argv[1]);

     yyin = fp;
     yyparse();
     return 0;
}

