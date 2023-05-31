structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"
val ENV = ref [("0",IntVal 0)];
val TENV = ref [("0",INT)];

fun evalExp(e:exp, env:environment ref):value =
    case e of
	NumExp i            => IntVal i
      | Bool b         => BoolVal b
      | VarExp x            => envLookup (x, env) 
      | UnExp (b,e1)        => evalUnExp(b,e1,env)				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | IfExp (e1,e2,e3)    => if 
    let
        val y = evalExp(e1,env) 
    in
        case y of 
           BoolVal b => b
        |  _ => raise brokenTypes
    end 
    then evalExp(e2,env) else evalExp(e3,env)
      | LetExp(ValDecl(x,Fn(id,typ1,typ2,e1)), e2)  =>
    let
      val env11 = ref ((x,FunVal(id,e1))::(!env))
    in
	    evalExp(e2, env11)
    end
      | AppExp (VarExp x , e1) => 
    let
        val y = evalExp (e1,env) ;
    in
        case envLookup (x,env) of 
           FunVal (id,e2) =>let val env23 = ref ((id,y)::(!env)) in
                evalExp (e2,env23) end
        | _ => raise brokenTypes
    end
      | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env)
	  in
	    evalExp(e2, envAdd (x, v1, env))
    end
      | Fun(id1,id2,typ1,typ2,e1) =>
        envLookup(id1,envAdd(id1,FunVal(id2,e1),env))
      |_ => raise brokenTypes

and
evalCode(p:PROGRAM) = evalProg(p,ENV)
and
evalProg(p:PROGRAM,env:environment ref)=
    if p = nil then ( ENV :=[("0",IntVal 0)];nil)
    else evalExp(hd p,env)::evalProg(tl p,env)
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment ref):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (EQUALS, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
  |   (EQUALS,BoolVal b1,BoolVal b2) => BoolVal (b1 = b2)
  |   (AND, BoolVal b1,BoolVal b2) => BoolVal (b1 andalso b2)
  |   (OR, BoolVal b1,BoolVal b2) => BoolVal (b1 orelse b2)
  |   (XOR,BoolVal b1,BoolVal b2) => BoolVal (not (b1 andalso b2))
  |   (GREATERTHAN, IntVal i1, IntVal i2) => BoolVal (i1>i2)
  |   (LESSTHAN, IntVal i1 , IntVal i2) => BoolVal (i1<i2)
  |   (IMPLIES, BoolVal b1,BoolVal b2) => BoolVal ( if b1 = true andalso b2 = false then false else true )			    
and
evalUnExp(b:unop,e1:exp,env:environment ref):value = 
case (b,evalExp(e1,env)) of 
    (NOT,BoolVal b1)  => BoolVal (not b1)
  | (NEGATE,IntVal i1) => IntVal (0-i1)
and
typeChecker(q:PROGRAM)= if length (typeCheck(q,TENV)) > 0 then "All the types match well so the code is properly typed , Proceed to evaluation :):):)" else "I'm annihilated"
and
typeList(q:PROGRAM) = typeCheck(q,TENV)
and
typeCheck(q:PROGRAM,tenv:typenvironment ref)=
   if q = nil then (TENV := [("0",INT)];nil)
   else typeExp(hd q,tenv)::typeCheck(tl q,tenv)
and
typeExp(e:exp,tenv:typenvironment ref)=
case e of 
NumExp i => INT
  | Bool b => BOOL
  | VarExp x => tenvLookup(x,tenv)
  | BinExp(b,e1,e2) => typeBinExp(b,e1,e2,tenv)
  | UnExp(NOT,e1) => if typeExp(e1,tenv)=BOOL then BOOL else raise Fail " type of operands doesnt agree with 'NOT' expression "
  | UnExp(NEGATE,e1) => if typeExp(e1,tenv)=INT then INT else raise Fail "type of operands doesnt agree with 'Negate' expression"
  | IfExp(e1,e2,e3) => if ((typeExp(e1,tenv)=BOOL)andalso((typeExp(e2,tenv)=BOOL)andalso(typeExp(e3,tenv)=BOOL))) then BOOL else if ((typeExp(e1,tenv)=BOOL)andalso((typeExp(e2,tenv)=INT)andalso(typeExp(e3,tenv)=INT))) then INT else raise Fail "type mismtch of operands of 'if' expression "
  | Fun(i1,i2,typ1,typ2,e1) => let val env12 = ref ((i2,typ1)::(i1,ARROW(typ1,typ2))::(!tenv)) in if typeExp(e1,env12)=typ2 then tenvLookup(i1,tenvAdd(i1,ARROW(typ1,typ2),tenv)) else raise Fail ("type mismatch in function declaration of "^i1) end
  | LetExp(ValDecl(x,Fn(i1,typ1,typ2,e1)),e2) =>
    let 
      val tenv1 = ref ((i1,typ1)::(!tenv))
    in
        if typeExp(e1,tenv1)=typ2 then typeExp(e2,tenvAdd(x,ARROW(typ1,typ2),tenv1)) else raise Fail "type mismatch in 'Let' Expression "
    end
  | AppExp (VarExp i,e1) =>
    let
      val t = typeExp(VarExp i,tenv)
    in
      case t of 
      ARROW(t1,t2)=> if (typeExp(e1,tenv)=t1) then t2 else raise Fail ("type mismatch in function application of  "^i)
    end
  | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val t = typeExp(e1,tenv)
	  in
	    typeExp(e2, tenvAdd (x, t,tenv))
    end
  | _ => raise brokenTypes
and
typeBinExp(b:binop,e1:exp,e2:exp,tenv:typenvironment ref) = 
case (b,e1,e2) of 
 (Add,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then INT else raise Fail "type of operands doesnt agree with 'PLUS' operator expression "
  | (Mul,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then INT else raise Fail "type of operands doesnt agree with 'TIMES' operator expression "
  | (Sub,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then INT else raise Fail "type of operands doesnt agree with 'MINUS' operator expression"
  | (AND,e1,e2) => if (typeExp(e1,tenv)=BOOL)andalso(typeExp(e2,tenv)=BOOL) then BOOL else raise Fail "type of operands doesnt agree with 'AND' operator expression"
  | (OR,e1,e2) => if (typeExp(e1,tenv)=BOOL)andalso(typeExp(e2,tenv)=BOOL) then BOOL else raise Fail " type of operands doesnt agree with 'OR' operator expression"
  | (XOR,e1,e2) => if (typeExp(e1,tenv)=BOOL)andalso(typeExp(e2,tenv)=BOOL) then BOOL else raise Fail " type of operands doesnt agree with 'XOR' operator expression"
  | (IMPLIES,e1,e2) => if (typeExp(e1,tenv)=BOOL)andalso(typeExp(e2,tenv)=BOOL) then BOOL else raise Fail "type of operands doesnt agree with 'IMPLIES' operator expression"
  | (GREATERTHAN,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then BOOL else raise Fail " type of operands doesnt agree with 'GREATERTHAN'  operator expression"
  | (LESSTHAN,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then BOOL else raise Fail " type of operands doesnt agree with 'LESSTHAN' operator expression"
  | (EQUALS,e1,e2) => if (typeExp(e1,tenv)=INT)andalso(typeExp(e2,tenv)=INT) then BOOL else if (typeExp(e1,tenv)=BOOL)andalso(typeExp(e2,tenv)=BOOL) then BOOL else raise Fail " type of operands doesnt agree with 'EQUALS' operator expression"


end

