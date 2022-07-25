/* mp2scan.l */
/* Copyright (c) 2021 J. M. Spivey */

%{
#include "mp2parse.h"

extern void panic(char *msg);

struct {
     char *name;
     int value;
} kwtable[] = {
     "curveto", CURVETO,
     "lineto", LINETO,
     "moveto", MOVETO,
     "showpage", SHOWPAGE,
     "newpath", NEWPATH,
     "fshow", FSHOW,
     "closepath", CLOSEPATH,
     "fill", FILL,
     "stroke", STROKE,
     "clip", CLIP,
     "rlineto", RLINETO,
     "setlinejoin", SETLINEJOIN,
     "setlinecap", SETLINECAP,
     "setmiterlimit", SETMITERLIMIT,
     "setgray", SETGRAY,
     "setrgbcolor", SETRGBCOLOR,
     "setdash", SETDASH,
     "gsave", GSAVE,
     "grestore", GRESTORE,
     "translate", TRANSLATE,
     "scale", SCALE,
     "concat", CONCAT,
     "dtransform", DTRANSFORM,
     "setlinewidth", SETLINEWIDTH,
     "pop", POP,
     "truncate", TRUNCATE,
     "exch", EXCH,
     "idtransform", IDTRANSFORM,
     NULL, 0
};

int lookup(char *s) {
     int i;

     for (i = 0; kwtable[i].name != NULL; i++) {
	  if (strcmp(s, kwtable[i].name) == 0)
	       return kwtable[i].value;
     }

     yylval.s = strdup(s);
     return NAME;
}

#define STRMAX 1024

char yystring[STRMAX];
int slen;

void save(char c) {
     if (slen >= STRMAX) panic("string too long");
     yystring[slen++] = c;
}

int yywrap(void) {
     return 1;
}

%}

%x STR
%x SKIP

%%

[A-Za-z][-A-Za-z0-9]+		{ return lookup(yytext); }
-?[0-9]+("."[0-9]+)?		{ yylval.d = atof(yytext); return NUMBER; }
"("				{ slen = 0; BEGIN(STR); }
"["				{ return BRA; }
"]"				{ return KET; }
"%"				{ BEGIN(SKIP); }
^"%%BoundingBox:"		{ return BBOX; }
^"%%HiResBoundingBox:" 		{ return HIRESBB; }
^("%%Creator:"|"%%CreationDate:").* { yylval.s = strdup(yytext); 
				  return COMMENT; }
[ \n]				;
.				{ return BADTOK; }

<STR>{
[^")"]				{ save(yytext[0]); }
"\\"[^0-7]			{ save(yytext[1]); }
"\\"[0-7][0-7][0-7]		{ save(strtoul(&yytext[1], NULL, 8)); }
")" 				{ BEGIN(INITIAL); 
				  yylval.len = slen; return STRING; }
}

<SKIP>.*\n			{ BEGIN(INITIAL); }
