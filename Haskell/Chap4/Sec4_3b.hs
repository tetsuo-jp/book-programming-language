module Sec4_3b where

import SynLib
import ParseLib
import ScanLib
import Data.Char (ord,isAlphaNum,isAlpha,isSpace)

import Prelude hiding (seq,lex)

-- p.47
numval :: [Char] -> Int
numval = foldl (\u l -> u * 10 + l) 0 . map digval

digval :: Char -> Int
digval d = ord d - ord '0'

-- p.76, p.68-69 (<-- p.63, p.53)
data Expr = Num Numeral
          | Var Ide
          | Bexpr BinOpr Expr Expr
          | Let [Decl] Expr
             deriving Show
data Decl = Decl Ide Expr
          deriving Show
type Numeral = [Char]
type Ide = [Char]
data BinOpr = Plus | Minus | Times | Over
            deriving Show

-- p.69
type Val = Int

-- p.70
type Env = Assoc Ide Val

none :: Assoc a b               -- p.38
assoc :: b -> Assoc a b         -- p.42
aLookup :: Eq a => Assoc a b -> a -> b -- p.41
update :: Eq a => Assoc a b -> a -> b -> Assoc a b -- p.41

type Assoc a b = [(a,b)]        -- p.41
none = []                       -- p.41-42
assoc d = [(undefined,d)]       -- p.42
aLookup [] x = undefined        -- p.41-42
aLookup [(_,d)] x = d           -- p.41-42
aLookup ((y,v):h) x | x==y = v
                    | otherwise = aLookup h x
update h x v = (x,v) : h        -- p.42

-- p.71
declenv :: Decl -> Env -> Env -> Env
declenv (Decl x e ) r' r = update r x (expval e r')

-- p.77, p.71
expval :: Expr -> Env -> Int
expval (Num n) r        = numval n
expval (Var x) r        = aLookup r x
expval (Bexpr o e e') r = binopr o (expval e r) (expval e' r)
expval (Let ds e) r     = expval e (recdecl ds r)

-- p.77
recdecl :: [Decl] -> Env -> Env
recdecl ds r = r''
               where
               r'' = foldl declenv' r ds
               declenv' r' d = declenv d r'' r'

-- p.72
binopr :: BinOpr -> Int -> Int -> Int
binopr Plus  = (+)
binopr Minus = (-)
binopr Times = (*)
binopr Over  = div

-- p.62
lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "let", T_Sym),
                (string "in", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")",
                               "+","-", "*", "/",
                               "=",";"
                               ], T_Sym) ]

-- p.64, p.72
r0 :: Env
r0 = none

eval :: String -> Int
eval = flip expval r0 . parse

{---------------------------------------------------------
  補助関数
 ---------------------------------------------------------}

type Prog = Expr
prog = expr

expr :: Parser (Tag,[Char]) Expr
expr =  lit "let" `x_seq` decls `seqp`
                lit "in" `x_seq` expr
                        `using` uncurry Let `alt`
        rexpr
rexpr = expr'' `seqp` rexpr' `using` uncurry (flip id)
rexpr'= succeed id
expr''=	term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
              (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
                (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
--factor =some factor' `using` foldl1 Apply
factor = factor'
factor'=kind T_Num `using` Num `alt`
        kind T_Ide `using` Var `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"

decls :: Parser (Tag,[Char]) [Decl]
decls = decl `seqp` many (lit ";" `x_seq` decl) `using` uncurry (:)

decl :: Parser (Tag,[Char]) Decl
decl =	(kind T_Ide `seqp` lit "=" `x_seq` expr)
                `using` uncurry Decl

{---------------------------------------------------------
  テスト
 ---------------------------------------------------------}

-- p.69
testexp01 = Bexpr Plus (Num "29") (Num "31")
testexp02 = Bexpr Times (Var "x") (Num "3")
testexp03 =
    Let [Decl "x" (Bexpr Plus (Num "29") (Num "31"))]
        (Bexpr Minus (Bexpr Times (Var "x") (Num "3"))
                     (Bexpr Times (Var "x") (Var "x")) )

-- p.69
test01 = parse "29+31"
test02 = parse "x*3"
test03 = parse "let x = 29 in 1"
test04 = parse "let x = 29 + 31 in x * 3 - x * x"
test05 = eval "let x = 29 + 31 in x * 3 - x * x" -- p.73

-- p.75
test10 = parse "let x = 1 in let y = x; x = 2 in x + y"
test11 = eval "let x = 1 in let y = x; x = 2 in x + y" -- ==> 4
test12 = eval "let x = 1 in let y = z; z = x in x + y" -- ==> 2