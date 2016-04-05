module Chap9 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib


-- Chapter 9
-- Generation of Reverse Polish code for expressions and Continuation code
-- for control structures.

-- Stack represented by a list
type Stack = [Val]
type Store = Assoc Ide Val
data Val = V_Int Int | V_Bool Bool deriving Show

-- State represented by  Stack and Store
type State = (Stack,Store)

-- Instruction
data Instr = PushNum Int | PushVar Ide | PopVar Ide
           | Bopr BinOpr | Ropr RelOpr
           | Jump Code | JumpFalse Code | Stop
             deriving Show

type Code = [Instr]

-- Code generation
gencmd :: Cmd -> Code -> Code
gencmd (Assign x e) is = genexp e ++ [PopVar x] ++ is
gencmd (Seq cs) is = foldr gencmd is cs
gencmd (If e (c,c')) is =
        genexp e ++ [JumpFalse is'] ++ gencmd c [Jump is] ++ is'
        where
        is' = gencmd c' is
gencmd (While e c) is = is'
        where
        is' = genexp e ++ [JumpFalse is] ++ gencmd c [Jump is'] ++ is
gencmd Skip p = p

genexp :: Expr -> Code
genexp (Num n) = [PushNum (numval n)]
genexp (Var x) = [PushVar x]
genexp (Bexpr o e e') = genexp e ++ genexp e' ++ [Bopr o]
genexp (Rexpr o e e') = genexp e ++ genexp e' ++ [Ropr o]

-- Code generator
compile s = gencmd (parse s) [Stop]

-- Section 9.4
-- Stack machine with transfer instructions

-- Initial Stack
st0 :: Stack
st0 = []

-- Initial Store
t0 :: Store
t0 = none

-- Execute instructions
exec :: Code -> State -> State
exec (Stop:_) s = s
exec (PushNum n:is) (st,t) = exec is (V_Int n:st, t)
exec (PushVar x:is) (st,t) = exec is (lookup t x : st, t)
exec (PopVar x:is) (v:st,t) = exec is (st, update t x v)
exec (Bopr o:is) (V_Int v':V_Int v:st,t) = exec is (V_Int(binopr o v v'):st, t)
exec (Ropr o:is) (V_Int v':V_Int v:st,t) = exec is (V_Bool(relopr o v v'):st, t)
exec (Jump is':_) (st,t) = exec is' (st,t)
exec (JumpFalse is':is) (V_Bool v:st,t)
        | v = exec is (st,t)
        | otherwise = exec is' (st,t)

-- Run code
runcode is = lookup (snd (exec is (st0,t0))) "result"

-- Example program
example =
  "begin n:=3; f:=1; while n>0 do begin f:=n*f; n:=n-1 end; result:=f end"

-- Chapter 9
-- Compilation

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "if", T_Sym),
                (string "then", T_Sym),
                (string "else", T_Sym),
                (string "while", T_Sym),
                (string "do", T_Sym),
                (string "skip", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")","+","-","*","/",":=",";",
                       "==","/=",">=",">","<=","<"], T_Sym)
                ]

-- Syntactic domains
data Cmd = Assign Ide Expr | Seq [Cmd] | If Expr (Cmd,Cmd)
         | While Expr Cmd | Skip
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr

cmd  =   lit "begin" `x_seq` cmds `seq_x` lit "end" `using` Seq
   `alt` lit "if" `x_seq` expr `seqp`
         (lit "then" `x_seq` cmd `seqp` (lit "else" `x_seq` cmd))
         `using` uncurry If
   `alt` lit "while" `x_seq` expr `seqp` (lit "do" `x_seq` cmd)
         `using` uncurry While
   `alt` lit "skip" `using` const Skip
   `alt` kind T_Ide `seqp` lit ":=" `x_seq` expr
         `using` uncurry Assign
cmds = cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)
expr = expr' `seqp` rexpr `using` uncurry (flip id)
rexpr=   lit "==" `x_seq` expr' `using` flip (Rexpr Equal)
   `alt` lit "/=" `x_seq` expr' `using` flip (Rexpr NotEqual)
   `alt` lit ">" `x_seq` expr' `using` flip (Rexpr Greater)
   `alt` lit ">=" `x_seq` expr' `using` flip (Rexpr GreaterEq)
   `alt` lit "<" `x_seq` expr' `using` flip (Rexpr Less)
   `alt` lit "<=" `x_seq` expr' `using` flip (Rexpr LessEq)
   `alt` succeed id
expr'= term `seqp` expr'' `using` uncurry (foldl (flip id))
expr''= many ((lit "+" `x_seq` term `using` flip (Bexpr Plus))
        `alt` (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term = factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times))
        `alt` (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =   kind T_Num `using` Num
     `alt` kind T_Ide `using` Var
     `alt` lit "(" `x_seq` expr `seq_x` lit ")"

-- Parser
type Prog = Cmd
prog = cmd

lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

-- Testing
test1 = runcode $ compile example