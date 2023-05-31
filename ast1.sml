structure AST =
struct

type id = string

datatype typ = ARROW of typ * typ 
			 | INT 
			 | BOOL 

datatype binop = Add | Sub | Mul |  EQUALS | AND | OR | XOR | GREATERTHAN | LESSTHAN | IMPLIES

datatype unop = NEGATE | NOT  

datatype decl = ValDecl of id * exp

and exp = NumExp of int
    	| VarExp of id
        | Bool of bool
	    | BinExp of binop * exp * exp
        | UnExp of unop * exp
	    | LetExp of decl * exp
        | IfExp of exp * exp * exp
		| AppExp of exp * exp 
		| Fn of id * typ * typ * exp
		| Fun of  id * id * typ * typ *exp

type PROGRAM = exp list
		       
datatype value = IntVal of int
	           | BoolVal of bool
			   | FunVal of id * exp 
				
type environment = (id * value) list

type typenvironment = (id * typ) list

fun envAddH (var:id, v:value, env:environment ref) =
let
	val d =  ref ((var,v)::(!env))
in
	env := !d
end

fun envAdd(var :id,v:value,env:environment ref) = 
let
	val absd = envAddH(var,v,env)
in
	env
end

fun tenvAddH (var:id, v:typ, env:typenvironment ref) =
let
	val d =  ref ((var,v)::(!env))
in
	env := !d
end

fun tenvAdd(var :id,v:typ,env:typenvironment ref) = 
let
	val abs = tenvAddH(var,v,env)
in
	env
end

(*fun fenvAdd (var1 : fid, var2 : id, e : exp, fenv:funenvironment) = 
    (var1,var2,e)::fenv*)

fun envLookup (var:id, env:environment ref) =
    case List.find(fn (x, _) => x = var) (!env) of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Environment lookup error"^" "^var)			

fun tenvLookup (var:id, env:typenvironment ref) =
    case List.find(fn (x, _) => x = var) (!env) of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Environment lookup error"^" "^var)			    

(*fun fenvLookup1 (var : fid , fenv : funenvironment) : id = 
	case List.find(fn (x, _) => x = var) env of
				       SOME (x, y,z)   => y
				    |   NONE => raise Fail "Environment lookup error"

fun fenvLookup2 (var : fid , fenv : funenvironment) : exp = 
	case List.find(fn (x, _) => x = var) env of
				       SOME (x, y,z)   => z
				    |   NONE => raise Fail "Environment lookup error"*)						    
end
fun read_file (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	             SOME line => line :: loop instream
    	    	   | NONE      => []
    in
	 loop instream before TextIO.closeIn instream
    end;
fun convert(strl)=
    if strl = nil then ""
    else (hd strl)^(convert(tl strl));


