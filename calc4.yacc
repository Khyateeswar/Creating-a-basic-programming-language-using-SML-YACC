(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name Calc

%term
  ID of string | NUM of int | BOOL of string 
| PLUS | TIMES | MINUS | NEGATE  | RPAREN | LPAREN | EOF
| EQUALS | LET | IN | END | VAL | AND | IF | THEN | ELSE | IMPLIES | NOT | LESSTHAN | GREATERTHAN | EQ | OR | XOR | TERM | FI | TYP of string | TO | ASSIGN | FN |FUN | COL

%nonterm EXP of AST.exp | START of AST.PROGRAM | DECL of AST.decl | STMT of AST.PROGRAM | FUNEXP of AST.exp | TYPE of AST.typ

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)
%nonassoc EQ
%right IF THEN ELSE 
%left EQUALS GREATERTHAN LESSTHAN 
%left MINUS PLUS AND OR XOR 
%left TIMES IMPLIES
%right NOT NEGATE
%right RPAREN
%left LPAREN
%left TERM
(* %right *)
%start START

%verbose

%%


  START: STMT (STMT)

  STMT : EXP TERM STMT (((EXP1::nil))@(STMT))
  | EXP ((EXP1::nil))

  DECL: VAL ID EQ EXP (AST.ValDecl(ID, EXP))
  |  ID EQ FUNEXP (AST.ValDecl(ID,FUNEXP))

  EXP : NUM (AST.NumExp(NUM))
  | BOOL ( if BOOL = "TRUE" then AST.Bool(true) else AST.Bool(false) )
  | ID (AST.VarExp(ID))
  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS  EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES  EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))
  | NEGATE EXP (AST.UnExp(AST.NEGATE,EXP1))
  | LPAREN EXP RPAREN (EXP)
  | LET DECL IN EXP END (AST.LetExp(DECL, EXP1))
  | IF EXP THEN EXP ELSE EXP FI (AST.IfExp(EXP1,EXP2,EXP3))
  | EXP IMPLIES EXP (AST.BinExp(AST.IMPLIES, EXP1,  EXP2))
  | EXP OR  EXP (AST.BinExp(AST.OR, EXP1,  EXP2))
  | EXP AND  EXP (AST.BinExp(AST.AND,  EXP1, EXP2))
  | EXP XOR  EXP (AST.BinExp(AST.XOR,  EXP1, EXP2))
  | EXP EQUALS EXP (AST.BinExp(AST.EQUALS, EXP1, EXP2))
  | EXP GREATERTHAN EXP ( AST.BinExp(AST.GREATERTHAN, EXP1, EXP2))
  | EXP LESSTHAN EXP (AST.BinExp(AST.LESSTHAN, EXP1, EXP2))
  | NOT EXP ( AST.UnExp(AST.NOT,EXP1))
  | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1,EXP2))
  | FUN ID LPAREN ID COL TYPE RPAREN COL TYPE ASSIGN EXP ( AST.Fun(ID1,ID2,TYPE1,TYPE2,EXP1))

  FUNEXP : FN LPAREN ID COL TYPE RPAREN COL TYPE ASSIGN EXP (AST.Fn(ID1,TYPE1,TYPE2,EXP))

  TYPE : TYP ( if TYP = "int" then AST.INT else AST.BOOL )
  | TYPE TO TYPE (AST.ARROW(TYPE1,TYPE2))
  | LPAREN TYPE  RPAREN ( TYPE)

    
  

  
  

  
  
  

