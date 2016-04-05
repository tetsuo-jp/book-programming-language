module Sec5_4 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain

-- Section 5.4
-- Variable and array declarations (Exercise 5.5)

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign e e') r s@(l,t) = -- p.103
                (l, update t k (expval e' r s))
                where
                V_Loc k = explval e r s
cmdexec (Block ds cs) r s@(l,t) = (l, t')
        where
        (r',s') = foldl (flip vardecl) (r,s) ds -- p.98-100
        (_,t') = foldl (flip (flip cmdexec r')) s' cs

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
                              where
                              (l',s') = alloc s (numval n)

-- Expression (R-value)
expval :: Expr -> Env -> State -> Val
expval (Num n) r s = V_Int (numval n)
expval (Var x) r (_,t) = lookup t k
                         where
                         V_Loc k = lookup r x
expval (ArrElem x e) r s@(_,t) = lookup t (k + v) -- p.102
                                 where
                                 V_Loc k = lookup r x
                                 V_Int v = expval e r s
expval (Bexpr o e e') r s = V_Int (binopr o v v')
                            where
                            V_Int v = expval e r s
                            V_Int v' = expval e' r s
expval (Rexpr o e e') r s = V_Bool (relopr o v v')
                            where
                            V_Int v = expval e r s
                            V_Int v' = expval e' r s

-- Expression (L-value), p.102
explval :: Expr -> Env -> State -> Val
explval (Num n) r s        = undefined
explval (Var x) r s        = lookup r x
explval (ArrElem x e) r s  = V_Loc (k + v)
                             where
                             V_Loc k = lookup r x
                             V_Int v = expval e r s
explval (Bexpr o e e') r s = undefined
explval (Rexpr o e e') r s = undefined

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

-- Section 5.4
-- Variable and array declarations (Exercise 5.5)

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "var", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")","+","-","*","/",
                                ":=",",",";","[","]"],
                        T_Sym) ]

-- Syntactic domains
-- Assign Ide Expr is replaced by Assign Expr Expr
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] deriving Show
data Decl = VarDecl Ide | ArrDecl Ide Numeral deriving Show
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr
          | Bexpr BinOpr Expr Expr | Rexpr RelOpr Expr Expr
            deriving Show

-- Syntax analysis
decls = many (decl `seq_x` lit ";") `using` concat
decl = lit "var" `x_seq`
        (decl' `seqp` many (lit "," `x_seq` decl') `using` uncurry (:))
decl' = kind T_Ide `seqp` lit "[" `x_seq`
                kind T_Num `seq_x` lit "]"
                        `using` uncurry ArrDecl `alt`
        kind T_Ide `using` VarDecl
cmd =	lit "begin" `x_seq` decls `seqp` seqcmd `seq_x` lit "end"
                `using` uncurry Block `alt`
        expr `seqp` lit ":=" `x_seq` expr
                `using` uncurry Assign
seqcmd= cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)
expr =	term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
              (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
              (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
        kind T_Ide `seqp` indexed `using` uncurry (flip id) `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"
indexed=lit "[" `x_seq` expr `seq_x` lit "]" `using` flip ArrElem `alt`
        succeed Var

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
test1 = exec "begin var a,b[2]; a:=1; b[0]:=2; b[1]:=3; result:=b[0]+b[1] end"
test2 = exec "begin var b; a[0]:=1; a[1]:=2; result:=a[0]+a[1] end"
