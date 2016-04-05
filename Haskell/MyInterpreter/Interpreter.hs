module Interpreter where

import Data.Char (ord,isAlphaNum,isAlpha,isSpace)
import qualified Parser as P
import Syntax
import qualified Lexer as L
import Prelude hiding (lex)

expval                  :: Expr -> Env -> Val
expval (Num n) r         = V_Int (numval n)
expval (Var x) r         = aLookup r x
expval (Bexpr op e e') r = V_Int (binopr op v v')
        where V_Int v  = expval e r
              V_Int v' = expval e' r
expval (Let ds e) r     = expval e (recdecl ds r)
expval (Rexpr o e e') r = V_Bool (relopr o v v')
        where V_Int v  = expval e r
              V_Int v' = expval e' r
expval (If e (e',e'')) r
    | v         = expval e' r
    | otherwise = expval e'' r
        where V_Bool v = expval e r
expval (Fun x e) r = V_Fun f
        where f v = expval e (update r x v)
expval (Apply e e') r = f (expval e' r)
        where V_Fun f = expval e r
expval (E_Bool v) r = V_Bool (boolean v)
        where boolean "True"  = True
              boolean "False" = False

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

-- Arithmetic operations
binopr :: BinOpr -> Int -> Int -> Int
binopr Plus = (+)
binopr Minus = (-)
binopr Times = (*)
binopr Over = div

-- Relational operations
relopr          :: RelOpr -> Int -> Int -> Bool
relopr Equal     = (==)
relopr NotEqual  = (/=)
relopr Greater   = (>)
relopr GreaterEq = (>=)
relopr Less      = (<)
relopr LessEq    = (<=)

-- Parser
type Prog = Expr

lex :: [Char] -> [L.Token]
lex = L.lexer

syn :: [L.Token] -> Prog
syn = P.parse

parse :: [Char] -> Prog
parse = syn . L.lexer

-- Initial environment
r0 :: Env
r0 = none

-- Evaluation function
eval e = expval (parse e) r0