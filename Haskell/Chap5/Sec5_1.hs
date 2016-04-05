module Sec5_1 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib

-- Section 5.1
-- Fig.5.2: Assignment and sequencing commands

data Val = V_Int Int deriving Show -- p.89
type Store = Assoc Ide Val      -- p.86

cmdexec :: Cmd -> Store -> Store -- p.88-89
cmdexec (Assign x e) s = update s x (expval e s)
cmdexec (Seq cs) s = foldl (flip cmdexec) s cs

expval :: Expr -> Store -> Val  -- p.89
expval (Num n) s = V_Int (numval n)
expval (Var x) s = lookup s x
expval (Bexpr o e e') s = V_Int (binopr o v v')
                          where
                          V_Int v = expval e s
                          V_Int v' = expval e' s


-- Initial store, p.90
t0 :: Store
t0 = none

-- Execution function
exec c = lookup (cmdexec (parse c) t0) "result"

-- Section 5.1
-- Fig.5.2: Assignment and sequencing commands

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")","+","-","*","/",":=",";"], T_Sym) ]

-- Syntactic domains
data Cmd = Assign Ide Expr | Seq [Cmd] -- p.88
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr -- p.89

-- Syntax analysis
cmd =	lit "begin" `x_seq` seqcmd `seq_x` lit "end" `using` Seq `alt`
        kind T_Ide `seqp` lit ":=" `x_seq` expr `using` uncurry Assign
seqcmd= cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)
expr =	term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
              (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
              (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
        kind T_Ide `using` Var `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"

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
test1 = exec "begin y:=1; result:=y+2 end" -- p.87