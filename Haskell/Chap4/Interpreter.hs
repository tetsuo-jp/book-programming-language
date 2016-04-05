module Interpreter where

import SynLib
import ParseLib
import ScanLib
import Data.Char (ord,isAlphaNum,isAlpha,isSpace)

import Prelude hiding (seq,lex)

data Expr = Num Numeral
          | BBool [Char]
          | Var Ide
          | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
          | Boolexpr BoolOpr Expr Expr
          | Bnot Expr
          | Let [Decl] Expr
          | If Expr (Expr,Expr)
          | Fun Ide Expr
          | Apply Expr Expr
            deriving Show
data Decl = Decl Ide Expr deriving Show
type Numeral = [Char]
type Ide = [Char]
data BinOpr = Plus | Minus | Times | Over
            deriving Show
data RelOpr = Equal | NotEqual | Greater | GreaterEq | Less | LessEq
            deriving Show
data BoolOpr = And | Or
            deriving Show
data Val = V_Int Int
         | V_Bool Bool
         | V_Fun (Val -> Val)
type Env = Assoc Ide Val

instance Show Val where
    showsPrec p (V_Int n) = showsPrec p n
    showsPrec p (V_Bool b) = showsPrec p b
    showsPrec p (V_Fun f) = showString "<function>"

-- Abstract type 'Assoc' defined in Chapter 2
none :: Assoc a b
assoc :: b -> Assoc a b
aLookup :: Eq a => Assoc a b -> a -> b
update :: Eq a => Assoc a b -> a -> b -> Assoc a b

-- Implementation in terms of lists of pairs
type Assoc a b = [(a,b)]
none = []
assoc d = [(undefined,d)]
aLookup [] x = undefined
aLookup [(_,d)] x = d
aLookup ((y,v):h) x | x==y = v
                    | otherwise = aLookup h x
update h x v = (x,v) : h

expval                   :: Expr -> Env -> Val
expval (Num n) r         = V_Int (numval n)
expval (BBool x) r       = V_Bool (boolval x)
expval (Var x) r         = aLookup r x
expval (Bexpr op e e') r = V_Int (binopr op v v')
    where V_Int v  = expval e r
          V_Int v' = expval e' r
expval (Let ds e) r      = expval e (recdecl ds r)
expval (Rexpr o e e') r  = V_Bool (relopr o v v')
    where V_Int v = expval e r
          V_Int v' = expval e' r
expval (Boolexpr o e e') r  = V_Bool (boolopr o v v')
    where V_Bool v = expval e r
          V_Bool v' = expval e' r
expval (Bnot e) r  = V_Bool (not v)
    where V_Bool v = expval e r
expval (If e (e',e'')) r | v         = expval e' r
                         | otherwise = expval e'' r
    where V_Bool v = expval e r
expval (Fun x e) r = V_Fun f
    where f v = expval e (update r x v)
expval (Apply e e') r = f (expval e' r)
    where V_Fun f = expval e r

recdecl :: [Decl] -> Env -> Env
recdecl ds r = r''
               where
               r'' = foldl declenv' r ds
               declenv' r' d = declenv d r'' r'


declenv :: Decl -> Env -> Env -> Env
declenv (Decl x e) r' r = update r x (expval e r')

-- Semantic functions
numval :: Numeral -> Int
numval = foldl1 (*+) . map digval
         where
         a *+ b = a * 10 + b

digval :: Char -> Int
digval a = ord a - ord '0'

boolval :: [Char] -> Bool
boolval "True" = True
boolval "False" = False

-- Arithmetic operations
binopr :: BinOpr -> Int -> Int -> Int
binopr Plus = (+)
binopr Minus = (-)
binopr Times = (*)
binopr Over = div

-- Relational operations
relopr           :: RelOpr -> Int -> Int -> Bool
relopr Equal     = (==)
relopr NotEqual  = (/=)
relopr Greater   = (>)
relopr GreaterEq = (>=)
relopr Less      = (<)
relopr LessEq    = (<=)

-- Boolean operations
boolopr     :: BoolOpr -> Bool -> Bool -> Bool
boolopr And = (&&)
boolopr Or  = (||)

-- Lexer
lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "let", T_Sym),
                (string "in", T_Sym),
                (string "if", T_Sym),
                (string "then", T_Sym),
                (string "else", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/","=",";"
                                ], T_Sym) ]

-- Syntax analysis
expr =  lit "if" `x_seq` expr `seqp`
                lit "then" `x_seq` expr `seqp`
                lit "else" `x_seq` expr
                        `using` uncurry If `alt`
        lit "let" `x_seq` decls `seqp`
                lit "in" `x_seq` expr
                        `using` uncurry Let `alt`
        rexpr
rexpr = expr'' `seqp` rexpr' `using` uncurry (flip id)
rexpr'=	lit "==" `x_seq` expr'' `using` flip (Rexpr Equal) `alt`
        lit "/=" `x_seq` expr'' `using` flip (Rexpr NotEqual) `alt`
        lit ">" `x_seq` expr'' `using` flip (Rexpr Greater) `alt`
        lit ">=" `x_seq` expr'' `using` flip (Rexpr GreaterEq) `alt`
        lit "<" `x_seq` expr'' `using` flip (Rexpr Less) `alt`
        lit "<=" `x_seq` expr'' `using` flip (Rexpr LessEq) `alt`
        succeed id
expr''=	term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
              (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
                (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =some factor' `using` foldl1 Apply
factor'=kind T_Num `using` Num `alt`
        kind T_Ide `using` Var `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"
decls = decl `seqp` many (lit ";" `x_seq` decl) `using` uncurry (:)
decl =	kind T_Ide `seqp`
                (many (kind T_Ide) `seqp` lit "=" `x_seq` expr
                        `using` uncurry (flip (foldr Fun)))
                `using` uncurry Decl

-- Parser
type Prog = Expr
prog = expr

lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

-- Initial environment
r0 :: Env
r0 = none

-- Evaluation function
eval e = expval (parse e) r0