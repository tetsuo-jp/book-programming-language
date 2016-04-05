module Sec5_3 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain

-- Section 5.3
-- Fig.5.7: Variable declarations and storage allocation, p.99

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign x e) r s@(l,t) = (l, update t k (expval e r s))
                where
                V_Loc k = lookup r x
cmdexec (Block ds cs) r s@(l,t) = (l, t')
        where
        (r',s') = foldl (flip vardecl) (r,s) ds -- p.98-100
        (_,t') = foldl (flip (flip cmdexec r')) s' cs

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1

-- Expression (R-value)
expval :: Expr -> Env -> State -> Val
expval (Num n) r s = V_Int (numval n)
expval (Var x) r (_,t) = lookup t k
                         where
                         V_Loc k = lookup r x
expval (Bexpr o e e') r s = V_Int (binopr o v v')
                            where
                            V_Int v = expval e r s
                            V_Int v' = expval e' r s

-- Storage allocation
alloc :: State -> Int -> (Loc,State)
alloc (l,t) n = (l, (l+n,t'))
        where
        t' = foldl (flip flip undefined . update) t [l..l+n-1]


-- Initial store, p.90
t0 :: Store
t0 = none

-- Initial state
s0 :: State
s0 = (1, t0)

-- Initial environment
r0 :: Env
r0 = update none "result" (V_Loc 0)

-- Execution function
exec c = lookup (snd s) 0
         where
         s = cmdexec (parse c) r0 s0

-- Interactive execution
-- exec' = interact (show' . exec)

-- Section 5.3
-- Fig.5.7: Variable declarations and storage allocation

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "var", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")","+","-","*","/",":=",",",";"], T_Sym) ]

-- Syntactic domains
data Cmd = Assign Ide Expr | Block [Decl] [Cmd] -- p.96
           deriving Show
data Decl = VarDecl Ide         -- p.96
            deriving Show
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr -- p.89
            deriving Show

-- Syntax analysis
decls = many (decl `seq_x` lit ";") `using` concat
decl = lit "var" `x_seq`
        (decls' `seqp` many (lit "," `x_seq` decls') `using` uncurry (:))
decls' = kind T_Ide `using` VarDecl
cmd =	lit "begin" `x_seq` decls `seqp` seqcmd `seq_x` lit "end"
                `using` uncurry Block `alt`
        kind T_Ide `seqp` lit ":=" `x_seq` expr
                `using` uncurry Assign
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
test0 = exec "result := 3"
test1 = exec "begin result := 3 end"
test2 = exec "begin result := 3; result := result + 1 end"
test3 = exec "begin var n; result := 3; n:=5; result := n end"
test4 = exec "begin var n,k; result := 3; k:=3; n:=5; result := n+k end"