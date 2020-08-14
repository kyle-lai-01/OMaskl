(* Test Plan
   What we tested with OUnit testing: 
   - translation of string, boolean, math, and function ast expressions
     into corresponding OCaml code.
   - parsing and lexing of string, boolean, math, function,
     and unit expressions.
   - evaluation of string, boolean, math, function, and unit expressions. 

   What we tested manually: 
   - Main REPL commands
   - Translating OMaskl code in .txt file to OCaml code and creating .ml
     file containing that OCaml code.
   - Translating OMaskl code in .txt file to OCaml code and printing the
     resulting OCaml code in a REPL. 
   - Running OMaskl code in .txt file. 
   - Interpreting OMaskl expressions via OMaskl interpreter. 
   - Definitions in interpreter and running code: class, function, and variable
     definitions. 
   - Function, variable, math, boolean, string, unit expressions. 
   - Print and Print_endl functions. 

   We used OUnit testing for the translations of certain expressions because
   we could easily check the equality of the code that our translator was 
   producing and an expected output because the string representations were
   readily available. We also used OUnit testing for parsing and lexing of 
   expressions to test correctness because in the same way, we knew the proper
   syntax for the language and OUnit could check a large amount of different
   cases since we know the expected output. The evaluation of the expressions 
   were also tested using OUnit because the expected output and evaluation
   could be done by hand so OUnit could check the evaluation through the 
   functions we wrote with the expected outputs. Most OUnit tests were 
   written using glass box testing.

   We tested all that requires user input because in order to test 
   that the system properly accepts user input and provides the correct
   output both in format and content. We tested that the interpreting, 
   translating files and producing .ml files as output, 
   translating files and printing the result, and running files.
   We tested the evaluation of all types of expressions using the interpreter.
   We especially needed to test definitions using the interpreter in order
   to fully test their functionality of producing a new environment. This
   included function, variable, and class definitions. All object-oriented
   functionality was tested using the interpreter and file running. 

   We think this testing demonstrates that our system is correct because
   we thorougly tested individual sections of the system, such as the 
   evaluator and translator, using OUnit, as well as the full system
   thoroughly by manually running the system and testing many different
   situations. We tested the outputs against the expected outputs with test
   cases for all the different types that our system supports, including edge
   cases.
*)

open OUnit2
open Ast
open Translator
open Parser

let print s = s


(** [test_translate_string name expr expected_output] 
    constructs an OUnit test named [name] that 
    asserts the equality of [expected_output] with 
    [translate_string_expr expr].  *)
let test_translate_string
    (name : string)
    (expr : Ast.string_expr)
    (expected_output : string) : test =
  "Translator test for primitive string: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_string_expr expr)~printer:print)

(** [test_translate_concat name expr expected_output] 
      constructs an OUnit test named [name] that 
      asserts the equality of [expected_output] with 
      [translate_string_expr expr].  *)
let test_translate_concat
    (name : string)
    (expr : Ast.string_expr)
    (expected_output : string) : test =
  "Translator test for concatenation of strings: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_string_expr expr)~printer:print)

(** [test_translate_bool_binary name expr expected_output] 
      constructs an OUnit test named [name] that 
      asserts the equality of [expected_output] with 
      [translate_bool_expr expr].  *)
let test_translate_bool_binary
    (name : string)
    (expr : Ast.bool_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean operations: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_bool_expr expr)~printer:print)

(** [test_translate_bool_comparison name c a b expected_output] 
      constructs an OUnit test named [name] that 
      asserts the equality of [expected_output] with 
      [translate_bool_expr (Comparison(c, a, b))].  *)
let test_translate_bool_comparison
    (name : string)
    (c : Ast.bool_comparison)
    (a : Ast.math_expr)
    (b : Ast.math_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean comparisons: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_bool_expr (Comparison(c,a,b)))
        ~printer:print)

(** [test_translate_bool_others name expr expected_output] 
    constructs an OUnit test named [name] that 
    asserts the equality of [expected_output] with 
    [translate_bool_expr expr].  *)
let test_translate_bool_others
    (name : string)
    (expr : Ast.bool_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean comparisons: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_bool_expr expr) ~printer:print)

(** [test_translate_math name expr expected_output] 
        constructs an OUnit test named [name] that 
        asserts the equality of [expected_output] with 
        [translate_math_expr expr].  *)
let test_translate_math
    (name : string)
    (expr : Ast.math_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean operations: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_math_expr expr) ~printer:print)

(** [test_translate_math_binary name bin e1 e2 expected_output] 
    constructs an OUnit test named [name] that 
    asserts the equality of [expected_output] with 
    [translate_math_binary bin e1 e2].  *)
let test_translate_math_binary
    (name : string)
    (bin : Ast.math_binary)
    (e1 : Ast.math_expr)
    (e2 : Ast.math_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean operations: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_math_binary bin e1 e2) 
        ~printer:print)

(** [test_translate_math_unary name e expected_output] 
    constructs an OUnit test named [name] that 
    asserts the equality of [expected_output] with 
    [translate_math_expr e1].  *)
let test_translate_math_unary
    (name : string)
    (e : Ast.math_expr)
    (expected_output : string) : test =
  "Translator test for binary boolean operations: " ^ name >:: (fun _ ->
      assert_equal expected_output (translate_math_expr e) 
        ~printer:print)


(** [test_translate_func name expr expected_output] 
      constructs an OUnit test named [name] that 
      asserts the equality of [expected_output] with 
      [translate_func expr].  *)
let test_translate_func 
    (name : string)
    (expr : Ast.expr)
    (expected_output : string) : test = 
  "Translator test for functions" ^ name >:: (fun _ -> 
      assert_equal expected_output (translate_expr expr)~printer:print)

let translate_string_tests = [

  test_translate_string "empty" (Strung "\"\"") "\"\"";
  test_translate_string "different cases" (Strung "\"abcABC\"") "\"abcABC\"";
  test_translate_string "white space" 
    (Strung "\"   d e c o  y \"") "\"   d e c o  y \"";
  test_translate_string "variable" 
    (Stringvar( Var ("hi", "string"))) "hi";
  test_translate_string "variable conflict name" 
    (Stringvar( Var ("let", "string"))) "let_";

  test_translate_concat "empty empty" 
    (Concat ((Strung "\"\"" ),(Strung "\"\"")))"(\"\" ^ \"\")";
  test_translate_concat "A B" 
    (Concat ((Strung "\"A\"" ),(Strung "\"B\"")))"(\"A\" ^ \"B\")";
  test_translate_concat "Concat symbol" 
    (Concat ((Strung "\"^^^\""),(Strung "\"^ ^ \""))) "(\"^^^\" ^ \"^ ^ \")";
  test_translate_concat "empty variable" 
    (Concat ((Strung "\"\"" ),(Stringvar (Var ("x", "string"))))) 
    "(\"\" ^ x)";
  test_translate_concat "variable x variable y" 
    (Concat ((Stringvar (Var ("y", "string"))), 
             (Stringvar (Var ("x", "string"))))) "(y ^ x)";
  test_translate_concat "A variable y" 
    (Concat (Strung "\"A\"", (Stringvar (Var ("x", "string"))))) 
    "(\"A\" ^ x)";
]

let translate_bool_tests = [
  test_translate_bool_binary "And primitive" 
    (Boolbin(And, Boolean true, Boolean false))"((true) && (false))" ;
  test_translate_bool_binary "Or primitive" 
    (Boolbin(Or, Boolean true, Boolean false))"((true) || (false))" ;
  test_translate_bool_binary "Nand primitive" 
    (Boolbin(Nand, Boolean true, Boolean false))
    "((not ((true) && (false))))";
  test_translate_bool_binary "Nor primitive" 
    (Boolbin(Nor, Boolean true, Boolean false))
    "((not ((true) || (false))))";
  test_translate_bool_binary "Xor primitive" 
    (Boolbin(Xor, Boolean true, Boolean false))
    "( (fun x y -> (x || y) && (not (x && y))) (true) (false))";
  test_translate_bool_binary "And variable"
    (Boolbin(And, Boolvar(Var("boo","bool")), Boolvar(Var("foo","bool"))))
    "((boo) && (foo))" ;
  test_translate_bool_binary "Nested primitives"
    (Boolbin(And, Boolean true, (Boolbin(And, Boolean true, Boolean false))))
    "((true) && ((true) && (false)))";
  test_translate_bool_binary "Nested primitives 2"
    (Boolbin(And, Boolean false, (Boolbin(Or, Boolean false, Boolean false))))
    "((false) && ((false) || (false)))";
  test_translate_bool_binary "Nested variables"(
    Boolbin(And, Boolvar(Var("a","bool")),
            (Boolbin(Or, Boolvar(Var("b","bool")),
                     Boolvar(Var("c","bool"))))))
    "((a) && ((b) || (c)))";
  test_translate_bool_binary "Nested variables xor"(
    Boolbin(Xor, Boolvar(Var("a","bool")),
            (Boolbin(Or, Boolvar(Var("b","bool")),
                     Boolvar(Var("c","bool"))))))
    "( (fun x y -> (x || y) && (not (x && y))) (a) ((b) || (c)))";

  test_translate_bool_comparison "Primitive two ints equal" 
    Equal (Integer 0) (Integer 0) "(0. = 0.)" ;
  test_translate_bool_comparison "Primitive two floats equal" 
    Equal (Double 0.0) (Double 0.0) "(0. = 0.)" ;
  test_translate_bool_comparison "Primitive two ints less" 
    Less (Integer 0) (Integer 0) "(0. < 0.)" ;
  test_translate_bool_comparison "Primitive two floats less" 
    Less (Double 0.0) (Double 0.0) "(0. < 0.)" ;
  test_translate_bool_comparison "Primitive two ints greater" 
    Greater (Integer 0) (Integer 0) "(0. > 0.)" ;
  test_translate_bool_comparison "Primitive two floats greater" 
    Greater (Double 0.0) (Double 0.0) "(0. > 0.)" ;
  test_translate_bool_comparison "Primitive two ints Leq" 
    Leq (Integer 0) (Integer 0) "(0. <= 0.)" ;
  test_translate_bool_comparison "Primitive two floats Leq" 
    Leq (Double 0.0) (Double 0.0) "(0. <= 0.)" ;
  test_translate_bool_comparison "Primitive two ints Geq" 
    Geq (Integer 0) (Integer 0) "(0. >= 0.)" ; 
  test_translate_bool_comparison "Primitive two floats Geq" 
    Geq (Double 0.0) (Double 0.0) "(0. >= 0.)" ;
  test_translate_bool_comparison "Primitive two ints Neq" 
    Neq (Integer 0) (Integer 0) "(0. <> 0.)" ;
  test_translate_bool_comparison "Primitive two floats Neq" 
    Neq (Double 0.0) (Double 0.0) "(0. <> 0.)" ;
  test_translate_bool_comparison "Primitive negative floats equal"
    Equal (Double (-10.23)) (Double (-10.)) "(-10.23 = -10.)" ;
  test_translate_bool_comparison "Primitive negative floats Neq"
    Neq (Double (-10.23)) (Double (-10.)) "(-10.23 <> -10.)" ;
  test_translate_bool_comparison "Variables equal" 
    Equal (Mathvar (Var("x","ha"))) (Mathvar (Var("y","ha"))) "(x = y)" ;
  test_translate_bool_comparison "variables neq" 
    Neq (Mathvar (Var("x","ha"))) (Mathvar (Var("y","ha"))) "(x <> y)" ;

  test_translate_bool_others "Negation primitive"
    (Negation (Boolean true)) "(not (true))";
  test_translate_bool_others "Negation AND"
    (Negation (Boolbin(And, Boolean true, Boolean false)))
    "(not ((true) && (false)))" ;
  test_translate_bool_others "Boolean variable name conflict"
    (Boolvar( Var ("type", "bool"))) "(type_)";
]

let translate_math_tests = [
  test_translate_math "int" (Integer 0) "0.";
  test_translate_math "float" (Double 0.) "0.";
  test_translate_math "int" (Integer 4) "4.";
  test_translate_math "float" (Double 4.8) "4.8";
  test_translate_math "int" (Integer (-9)) "-9.";
  test_translate_math "float" (Double (-9.9)) "-9.9";
  test_translate_math "variable" (Mathvar (Var ("x", "int"))) "x";

  test_translate_math_binary "int + int" Plus (Integer 2) 
    (Integer 4) "(2. +. 4.)";
  test_translate_math_binary "int + float" Plus (Integer 2) 
    (Double 4.) "(2. +. 4.)";
  test_translate_math_binary "float + float" Plus (Double 2.) 
    (Double 4.) "(2. +. 4.)";
  test_translate_math_binary "negative int + int" Plus (Integer (-2)) 
    (Integer (-4)) "(-2. +. -4.)";
  test_translate_math_binary "negative int + float" Plus (Integer (-2))
    (Double (-4.)) "(-2. +. -4.)";
  test_translate_math_binary "negative float + float" Plus (Double (-2.)) 
    (Double (-4.)) "(-2. +. -4.)";
  test_translate_math_binary "negative variable + float" 
    Plus (Mathvar(Var("x", "float"))) (Double (-4.)) "(x +. -4.)";
  test_translate_math_binary "(int + int) + float" 
    Plus (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "((2. +. 3.) +. -4.)";

  test_translate_math_binary "int - int" Minus (Integer 2) 
    (Integer 4) "(2. -. 4.)";
  test_translate_math_binary "int - float" Minus (Integer 2) 
    (Double 4.) "(2. -. 4.)";
  test_translate_math_binary "float - float" Minus (Double 2.) 
    (Double 4.) "(2. -. 4.)";
  test_translate_math_binary "negative int - int" Minus (Integer (-2)) 
    (Integer (-4)) "(-2. -. -4.)";
  test_translate_math_binary "negative int - float" Minus (Integer (-2))
    (Double (-4.)) "(-2. -. -4.)";
  test_translate_math_binary "negative float - float" Minus (Double (-2.)) 
    (Double (-4.)) "(-2. -. -4.)";
  test_translate_math_binary "negative variable - float" 
    Minus (Mathvar(Var("x", "float"))) (Double (-4.)) "(x -. -4.)";
  test_translate_math_binary "(int + int) - float" 
    Minus (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "((2. +. 3.) -. -4.)";

  test_translate_math_binary "int * int" Times (Integer 2) 
    (Integer 4) "(2. *. 4.)";
  test_translate_math_binary "int * float" Times (Integer 2) 
    (Double 4.) "(2. *. 4.)";
  test_translate_math_binary "float * float" Times (Double 2.) 
    (Double 4.) "(2. *. 4.)";
  test_translate_math_binary "negative int * int" Times (Integer (-2)) 
    (Integer (-4)) "(-2. *. -4.)";
  test_translate_math_binary "negative int * float" Times (Integer (-2))
    (Double (-4.)) "(-2. *. -4.)";
  test_translate_math_binary "negative float * float" Times (Double (-2.)) 
    (Double (-4.)) "(-2. *. -4.)";
  test_translate_math_binary "negative variable * float" 
    Times (Mathvar(Var("x", "float"))) (Double (-4.)) "(x *. -4.)";
  test_translate_math_binary "(int + int) * float" 
    Times (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "((2. +. 3.) *. -4.)";

  test_translate_math_binary "int / int" Divide (Integer 2) 
    (Integer 4) "(2. /. 4.)";
  test_translate_math_binary "int / float" Divide (Integer 2) 
    (Double 4.) "(2. /. 4.)";
  test_translate_math_binary "float / float" Divide (Double 2.) 
    (Double 4.) "(2. /. 4.)";
  test_translate_math_binary "negative int / int" Divide (Integer (-2)) 
    (Integer (-4)) "(-2. /. -4.)";
  test_translate_math_binary "negative int / float" Divide (Integer (-2))
    (Double (-4.)) "(-2. /. -4.)";
  test_translate_math_binary "negative float / float" Divide (Double (-2.)) 
    (Double (-4.)) "(-2. /. -4.)";
  test_translate_math_binary "negative variable / float" 
    Divide (Mathvar(Var("x", "float"))) (Double (-4.)) "(x /. -4.)";
  test_translate_math_binary "(int + int) / float" 
    Divide (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "((2. +. 3.) /. -4.)";

  test_translate_math_binary "int / int" Modulo (Integer 2) 
    (Integer 4) "((int_of_float 2.) mod (int_of_float 4.))";
  test_translate_math_binary "int / float" Modulo (Integer 2) 
    (Double 4.) "((int_of_float 2.) mod (int_of_float 4.))";
  test_translate_math_binary "float / float" Modulo (Double 2.) 
    (Double 4.) "((int_of_float 2.) mod (int_of_float 4.))";
  test_translate_math_binary "negative int / int" Modulo (Integer (-2)) 
    (Integer (-4)) "((int_of_float -2.) mod (int_of_float -4.))";
  test_translate_math_binary "negative int / float" Modulo (Integer (-2))
    (Double (-4.)) "((int_of_float -2.) mod (int_of_float -4.))";
  test_translate_math_binary "negative float / float" Modulo (Double (-2.)) 
    (Double (-4.)) "((int_of_float -2.) mod (int_of_float -4.))";
  test_translate_math_binary "negative variable / float" 
    Modulo (Mathvar(Var("x", "float"))) (Double (-4.)) 
    "((int_of_float x) mod (int_of_float -4.))";
  test_translate_math_binary "(int + int) / float" 
    Modulo (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "((int_of_float (2. +. 3.)) mod (int_of_float -4.))";

  test_translate_math_binary "power int int" Power (Integer 2) 
    (Integer 4) "(Float.pow 2. 4.)";
  test_translate_math_binary "power int float" Power (Integer 2) 
    (Double 4.) "(Float.pow 2. 4.)";
  test_translate_math_binary "power float float" Power (Double 2.) 
    (Double 4.) "(Float.pow 2. 4.)";
  test_translate_math_binary "power negative int int" Power (Integer (-2)) 
    (Integer (-4)) "(Float.pow -2. -4.)";
  test_translate_math_binary "power negative int float" Power (Integer (-2)) 
    (Double (-4.)) "(Float.pow -2. -4.)";
  test_translate_math_binary "power negative float float" Power (Double (-2.)) 
    (Double (-4.)) "(Float.pow -2. -4.)";
  test_translate_math_binary "power negative variable float" 
    Power (Mathvar(Var("x", "float"))) (Double (-4.)) "(Float.pow x -4.)";
  test_translate_math_binary "power (int + int) float" 
    Power (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "(Float.pow (2. +. 3.) -4.)";

  test_translate_math_binary "min int int" Min (Integer 2) 
    (Integer 4) "(min 2. 4.)";
  test_translate_math_binary "min int float" Min (Integer 2) 
    (Double 4.) "(min 2. 4.)";
  test_translate_math_binary "min float float" Min (Double 2.) 
    (Double 4.) "(min 2. 4.)";
  test_translate_math_binary "min negative int int" Min (Integer (-2)) 
    (Integer (-4)) "(min -2. -4.)";
  test_translate_math_binary "min negative int float" Min (Integer (-2)) 
    (Double (-4.)) "(min -2. -4.)";
  test_translate_math_binary "min negative float float" Min (Double (-2.)) 
    (Double (-4.)) "(min -2. -4.)";
  test_translate_math_binary "min negative variable float" 
    Min (Mathvar(Var("x", "float"))) (Double (-4.)) "(min x -4.)";
  test_translate_math_binary "min (int + int) float" 
    Min (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "(min (2. +. 3.) -4.)";

  test_translate_math_binary "max int int" Max (Integer 2) 
    (Integer 4) "(max 2. 4.)";
  test_translate_math_binary "max int float" Max (Integer 2) 
    (Double 4.) "(max 2. 4.)";
  test_translate_math_binary "max float float" Max (Double 2.) 
    (Double 4.) "(max 2. 4.)";
  test_translate_math_binary "max negative int int" Max (Integer (-2)) 
    (Integer (-4)) "(max -2. -4.)";
  test_translate_math_binary "max negative int float" Max (Integer (-2)) 
    (Double (-4.)) "(max -2. -4.)";
  test_translate_math_binary "max negative float float" Max (Double (-2.)) 
    (Double (-4.)) "(max -2. -4.)";
  test_translate_math_binary "max negative variable float" 
    Max (Mathvar(Var("x", "float"))) (Double (-4.)) "(max x -4.)";
  test_translate_math_binary "max (int + int) float" 
    Max (Binary (Plus, Integer 2, Integer 3)) (Double (-4.)) 
    "(max (2. +. 3.) -4.)";

  test_translate_math_unary "sqrt int" (Unary (Sqrt, Integer 2)) 
    "(Float.sqrt 2.)";
  test_translate_math_unary "sqrt float" (Unary (Sqrt, Double 2.)) 
    "(Float.sqrt 2.)";
  test_translate_math_unary "sqrt negative int" (Unary (Sqrt, Integer (-2))) 
    "(Float.sqrt -2.)";
  test_translate_math_unary "sqrt negative float" (Unary (Sqrt, Double (-2.))) 
    "(Float.sqrt -2.)";
  test_translate_math_unary "sqrt variable" 
    (Unary (Sqrt, Mathvar (Var ("x", "int")))) "(Float.sqrt x)";
  test_translate_math_unary "sqrt (int + int)" 
    (Unary (Sqrt, Binary(Plus, Integer 2, Integer 2))) 
    "(Float.sqrt (2. +. 2.))";

  test_translate_math_unary "abs int" (Unary (Abs, Integer 2)) 
    "(Float.abs 2.)";
  test_translate_math_unary "abs float" (Unary (Abs, Double 2.)) 
    "(Float.abs 2.)";
  test_translate_math_unary "abs egative int" (Unary (Abs, Integer (-2))) 
    "(Float.abs -2.)";
  test_translate_math_unary "abs negative float" (Unary (Abs, Double (-2.))) 
    "(Float.abs -2.)";
  test_translate_math_unary "abs variable" 
    (Unary (Abs, Mathvar (Var ("x", "int")))) "(Float.abs x)";
  test_translate_math_unary "abs (int + int)" 
    (Unary (Abs, Binary(Plus, Integer 2, Integer 2))) 
    "(Float.abs (2. +. 2.))";

  test_translate_math_unary "floor int" (Unary (Floor, Integer 2)) 
    "(Float.floor 2.)";
  test_translate_math_unary "floor float" (Unary (Floor, Double 2.)) 
    "(Float.floor 2.)";
  test_translate_math_unary "floor negative int" (Unary (Floor, Integer (-2))) 
    "(Float.floor -2.)";
  test_translate_math_unary "floor negative float" (Unary (Floor, Double (-2.))) 
    "(Float.floor -2.)";
  test_translate_math_unary "floor variable" 
    (Unary (Floor, Mathvar (Var ("x", "int")))) "(Float.floor x)";
  test_translate_math_unary "floor (int + int)" 
    (Unary (Floor, Binary(Plus, Integer 2, Integer 2))) 
    "(Float.floor (2. +. 2.))";

  test_translate_math_unary "ceil int" (Unary (Ceil, Integer 2)) 
    "(Float.ceil 2.)";
  test_translate_math_unary "ceil float" (Unary (Ceil, Double 2.)) 
    "(Float.ceil 2.)";
  test_translate_math_unary "ceil negative int" (Unary (Ceil, Integer (-2))) 
    "(Float.ceil -2.)";
  test_translate_math_unary "ceil negative float" (Unary (Ceil, Double (-2.))) 
    "(Float.ceil -2.)";
  test_translate_math_unary "ceil variable" 
    (Unary (Ceil, Mathvar (Var ("x", "int")))) "(Float.ceil x)";
  test_translate_math_unary "ceil (int + int)" 
    (Unary (Ceil, Binary(Plus, Integer 2, Integer 2))) 
    "(Float.ceil (2. +. 2.))";
]


let translate_func_tests = [
  test_translate_func "Function with int" 
    (Deffunc (Function ("intfunc", "n", Anonymous (
         [Var ("n1", "int"); Var ("n2", "int")], Mathexp (Integer 1))), 
              Mathexp (Integer 1)))
    "let rec intfunc = (fun n1 n2 -> 1.) in\n1.";
  test_translate_func "Function with bool" 
    (Deffunc (Function ("boolfunc", "n", Anonymous (
         [Var ("n1", "bool"); Var ("n2", "bool")], Boolexp (Boolean true))), 
              Boolexp (Boolean true)))
    "let rec boolfunc = (fun n1 n2 -> (true)) in\n(true)";
  test_translate_func "Anonymous function" 
    (Mathexp (Mathfunc (Anonymous (
         [ Var ("n1", "int"); Var ("n2", "int")], 
         Mathexp (Integer 1)), [Mathexp (Integer 1)] )))
    "((fun n1 n2 -> 1.) 1.)";
  test_translate_func "Function application" (Mathexp (Mathfunc (
      (Named (Var ("intfunc", "")), 
       [Mathexp (Integer 1); Mathexp (Integer 2)]))))
    "(intfunc 1. 2.)";
  test_translate_func "Function name conflict" 
    (Deffunc (Function ("rec", "n", Anonymous (
         [Var ("n1", "bool"); Var ("n2", "bool")], Boolexp (Boolean true))), 
              Boolexp (Boolean true))) 
    "let rec rec_ = (fun n1 n2 -> (true)) in\n(true)";
]


let empty = Eval.initialize_env

(** [eval env prog] is a helper function that evaluates the ast [program] with
    the given environment [env], returning a string*)
let eval env prog = prog |> Interp.parse 
                    |> Eval.eval_expr env |> Eval.string_of_value

(** [test_eval_unit name prog expected] constructs an OUnit test named [name] 
    that asserts the equality of [expected] with [eval empty prog]. *)
let test_eval_unit
    (name : string)
    (prog : string)
    (expected : string) : test =
  "Evaluator test for unit: " ^ name >:: (fun _ ->
      assert_equal expected (eval empty prog) ~printer: print)

(** [test_eval_float name prog expected] constructs an OUnit test named [name] 
    that asserts the equality of [string_of_float expected] with 
    [eval empty prog]. *)
let test_eval_float
    (name : string)
    (prog : string)
    (expected : float) : test =
  "Evaluator test for float: " ^ name >:: (fun _ ->
      assert_equal ("float : "^ (string_of_float expected)) (eval empty prog) 
        ~printer: print)

(** [test_eval_int name prog expected] constructs an OUnit test named [name] 
    that asserts the equality of [string_of_int expected] with
    [eval empty prog]. *)
let test_eval_int
    (name : string)
    (prog : string)
    (expected : int) : test =
  "Evaluator test for int: " ^ name >:: (fun _ ->
      assert_equal ("int : "^ (string_of_int expected)) (eval empty prog) 
        ~printer: print)

(** [test_eval_bool name prog expected] constructs an OUnit test named [name] 
    that asserts the equality of [string_of_bool expected] with 
    [eval empty prog]. *)
let test_eval_bool
    (name : string)
    (prog : string)
    (expected : bool) : test =
  "Evaluator test for bool: " ^ name >:: (fun _ ->
      assert_equal ("boolean : "^ (string_of_bool expected)) (eval empty prog) 
        ~printer: print)

(** [test_eval_string name prog expected] constructs an OUnit test named [name] 
    that asserts the equality of string representation of [expected] with 
    [eval empty prog]. *)
let test_eval_string
    (name : string)
    (prog : string)
    (expected : string) : test =
  "Evaluator test for bool: " ^ name >:: (fun _ ->
      assert_equal ("string : \""^expected^"\"") (eval empty prog) 
        ~printer: print)

let eval_unit_tests = [
  test_eval_unit "unit" "()" "unit : ()";
  test_eval_unit "unit" "func<unit> f := [int x] ->_-> {()} in 
  f [2]" "unit : ()";
]

let eval_float_tests = [
  test_eval_float "Number" "3.4" 3.4;
  test_eval_float "Negative number" "-10.23" (-10.23);
  test_eval_float "Variable def" "float x := 1.2 in x" 1.2;
  test_eval_float "Negative variable def" "float x := -0.2 in x" (-0.2);
  test_eval_float "Addition" "0.4 +_+ 0.5" 0.9;
  test_eval_float "3 way addition " "0.1 +_+ 0.2 +_+ 0.3" 0.6;
  test_eval_float "Parentheses addition" "(0.2+_+0.5)+_+0.1" 0.8;
  test_eval_float "Subtraction" "0.3-_-0.3" 0.0;
  test_eval_float "Subtract negative" "0.3-_-(-0.3)" 0.6;
  test_eval_float "Multiplication" "1.2 *_* 1.0" 1.2;
  test_eval_float "Multiplication big zero" "15715837.2 *_* 0.0" 0.0;
  test_eval_float "Division" "9.0 /_/ 9.0" 1.0;
  test_eval_float "Func" 
    "func<float>tri:=[float x] ->_-> {x *_* 3.0}in tri[3.0]"9.0;
  test_eval_float "Overwrite var def" "float x:=0.0 in float x:= 1.0 in x"1.0;
  test_eval_float "Abs float neg" "abs(-4.0)" 4.0;
  test_eval_float "Abs float pos" "abs(10.23)" 10.23;
]

let eval_int_tests = [
  test_eval_int "Number" "1023" 1023;
  test_eval_int "Negative Number" "-1023" (-1023);
  test_eval_int "Variable def" "int x := 3 in x" 3;
  test_eval_int "Negative variable def" "int xyz := -5 in xyz" (-5);
  test_eval_int "Addition" "500+_+523" 1023;
  test_eval_int "3 way addition" "1 +_+ 2 +_+ 3" 6;
  test_eval_int "Parentheses addition" "3 +_+ ((((4+_+5))))" 12;
  test_eval_int "Floor" "floor(3.3)" 3;
  test_eval_int "Ceil" "ceil(3.3)" 4;
  test_eval_int "Multiplication" "3 *_* 5" 15;
  test_eval_int "Mult by zero" "0 *_* 1048576" 0;
  test_eval_int "Division" "6 /_/ 2" 3;
  test_eval_int "Func app" 
    "func<int> square := [int s] ->_-> {s*_*s} in square[4]"16;
  test_eval_int "Anon func app" "[int s] ->_-> {s*_*s}[4]"16;
  test_eval_int "Overwrite def" "int a:=2 in int a:= 4 in a" 4 ;
  test_eval_int "Min" "min 1 2" 1;
  test_eval_int "Min neg" "min (-1) (-2)" (-2);
  test_eval_int "Max" "max 1 2" 2;
  test_eval_int "Max neg" "max (-1) (-2)" (-1);
  test_eval_int "Abs int" "abs (-4)" 4;
]

let eval_bool_tests = [
  test_eval_bool "Literal true" "true" true;
  test_eval_bool "Literal false" "false" false;
  test_eval_bool "LAND" "true &_& true" true;
  test_eval_bool "LAND 2" "true &_& false" false;
  test_eval_bool "LAND 3" "false &_& false" false;
  test_eval_bool "LOR" "true |_| true" true;
  test_eval_bool "LOR 2" "true |_| false" true;
  test_eval_bool "LOR 3" "false |_| false" false;
  test_eval_bool "Neg" "!_! true" false;
  test_eval_bool "XOR" "true &_| false" true;
  test_eval_bool "XOR 2" "true &_| true" false;
  test_eval_bool "Equal int" " 2 =_= 2" true;
  test_eval_bool "Equal int 2" " 2 =_= 102147238" false;
  test_eval_bool "Equal int addition" "2+_+10 =_= 12" true;
  test_eval_bool "Equal float" "2.0 =_= 2.0" true;
  test_eval_bool "Equal float 2" "2.0 =_= -2.0" false;
  test_eval_bool "Less int" "1<_<5" true;
  test_eval_bool "Greater int" "5>_>1" true;
  test_eval_bool "Leq int" "1<=_=<5" true;
  test_eval_bool "Leq int 2" "5<=_=<5" true;
  test_eval_bool "Geq int" "1023 >=_=> -6" true;
  test_eval_bool "Geq int 2" "0 >=_=> 0" true;
  test_eval_bool "Neq int" "1 !=_=! 4" true;
  test_eval_bool "Var def" "boolean hehe := true in hehe" true;
  test_eval_bool "Var def duplicate" 
    "boolean hehe := true in boolean hehe := false in hehe" false;
  test_eval_bool "Anon func app" "[boolean b] ->_-> {!_! b}[true]" false;
  test_eval_bool "Func app" 
    "func<boolean> decoy := [boolean b] ->_-> {!_! b} in decoy [false]" true;
]

let eval_string_tests = [
  test_eval_string "Literal" {|"haha"|} "haha";
  test_eval_string "Concat" {|"1"$_$"2"|} "12";
  test_eval_string "Concat space" {|"1"$_$" "|} "1 ";
]

let suite = 
  "Project test suite" >::: List.flatten 
    [translate_string_tests; translate_math_tests; translate_bool_tests; 
     translate_func_tests; eval_unit_tests; eval_float_tests; eval_int_tests;
     eval_bool_tests; eval_string_tests]

let _ = run_test_tt_main suite