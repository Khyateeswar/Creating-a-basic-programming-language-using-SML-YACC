CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast1.sml";
use "evaluator.sml";
use "calc4.yacc.sig";
use "calc4.yacc.sml";
use "calc4.lex.sml";
use "load-calc4.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
val finalAST = parseString o convert o read_file ; 
open EVALUATOR;
val TypeCheck = typeChecker o finalAST;
val TypesList = typeList o finalAST;
val EvalFile  = evalCode o finalAST;
