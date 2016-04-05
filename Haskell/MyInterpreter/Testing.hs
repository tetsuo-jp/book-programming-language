module Testing where

import Interpreter
import Syntax

-- Testing
test1 = expval testexp1 none
testexp1 :: Expr -- let x = 29 + 31 in x * 3 - x * x
testexp1 = Let [Decl "x" (Bexpr Plus (Num "29") (Num "31"))]
	       (Bexpr Minus (Bexpr Times (Var "x") (Num "3"))
	                    (Bexpr Times (Var "x") (Num "x")) )

test2 = expval testexp2 none
testexp2 :: Expr -- if 1==1 then 2 else 3
testexp2 = If (Rexpr Equal (Num "1") (Num "1")) (Num "2",Num "3")

test3 = expval testexp3 none
testexp3 :: Expr -- let square = \x -> x * x in square 3
testexp3 = Let [Decl "square" (Fun "x" (Bexpr Times (Var "x") (Var "x")))]
	       (Apply (Var "square") (Num "3"))

test4 = expval testexp4 none
testexp4 :: Expr -- let x = 1 in let {y=x; x=2} in x + y
testexp4 = Let [Decl "x" (Num "1")]
	       (Let [Decl "y" (Var "x"), Decl "x" (Num "2")]
		    (Bexpr Plus (Var "x") (Var "y")))

test5 = expval testexp5 none
testexp5 :: Expr -- let x = 1 in let x=2; y=x in x + y
testexp5 = Let [Decl "x" (Num "1")]
	       (Let [Decl "x" (Num "2")]
		    (Bexpr Plus (Var "x") (Var "y")))

test6 = expval testexp6 none
testexp6 :: Expr -- let f n = if n == 0 then 1 else n * f (n-1) in f 5
testexp6 = Let [Decl "f" (Fun "n" (If (Rexpr Equal (Var "n") (Num "0")) (Num "1",Bexpr Times (Var "n") (Apply (Var "f") (Bexpr Minus (Var "n") (Num "1"))))))]
	       (Apply (Var "f") (Num "5"))

test7 = parse "1"
test8 = parse "square 1"
test9 = parse "let f x =  n in 5"
test10 = parse "let x = 29 + 31 in x * 3 - x * x"
test11 = parse "let f n = if n == 0 then 1 else n * f (n-1) in f 5"
test12 = eval "let f n = if n == 0 then 1 else n * f (n-1) in f 5"
test13 = eval "let x = 1 in (let x=2; y=x in x + y)" -- äáå ÇÕïKê{Ç≈Ç»Ç¢
test14 = eval "if 1 == 1 then 2 else 3"