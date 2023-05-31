structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


  val keywords =
  [
   ("IMPLIES",  Tokens.IMPLIES),
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("val",  Tokens.VAL),
   ("EQUALS", Tokens.EQUALS),
   ("if",Tokens.IF),
   ("fi",Tokens.FI),
   ("then",Tokens.THEN),
   ("else",Tokens.ELSE),
   ("AND",Tokens.AND),
   ("NOT",Tokens.NOT),
   ("NEGATE",Tokens.NEGATE),
   ("GREATERTHAN",Tokens.GREATERTHAN),
   ("LESSTHAN",Tokens.LESSTHAN),
   ("PLUS",Tokens.PLUS),
   ("MINUS",Tokens.MINUS),
   ("TIMES",Tokens.TIMES),
   ("OR",Tokens.OR),
   ("XOR",Tokens.XOR),
   ("fun",Tokens.FUN),
   ("fn",Tokens.FN)
   ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID (str, pos1, pos2)
  %%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
alphaNum = [A-Za-z0-9_];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
{alpha}{alphaNum}* => (if yytext = "TRUE" then Tokens.BOOL(yytext,!pos,!pos) else if yytext = "FALSE" then Tokens.BOOL(yytext,!pos,!pos) else if yytext = "int" then Tokens.TYP(yytext,!pos,!pos) else if yytext = "bool" then Tokens.TYP(yytext,!pos,!pos) else findKeywords(yytext,!pos,!pos));
"="      => (Tokens.EQ(!pos,!pos));
":"      => (Tokens.COL(!pos,!pos));
"=>"     => (Tokens.ASSIGN(!pos,!pos));
"->"     => (Tokens.TO(!pos,!pos));
";"      => (Tokens.TERM(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

