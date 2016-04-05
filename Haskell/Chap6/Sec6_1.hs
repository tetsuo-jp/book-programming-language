module Sec_6_1 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain


-- Section 6.1
-- Fig.6.2: Control structures

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign e e') r s@(l,t) =
        (l, update t k (expval e' r s))
        where
        V_Loc k = explval e r s
cmdexec (Block ds cs) r s@(l,t) = (l, t')
        where
        (r',s') = foldl (flip vardecl) (r,s) ds
        (_, t') = foldl (flip (flip cmdexec r')) s' cs
cmdexec (If e (c,c')) r s
        | v = cmdexec c r s
        | otherwise = cmdexec c' r s
        where
        V_Bool v = expval e r s
cmdexec c@(While e c') r s
        | v = cmdexec c r (cmdexec c' r s)
        | otherwise = s
        where
        V_Bool v = expval e r s
cmdexec Skip r s = s

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
expval (ArrElem x e) r s@(_,t) = lookup t (k + v)
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

-- Expression (L-value)
explval :: Expr -> Env -> State -> Val
explval (Num n) r s = undefined
explval (Var x) r s = lookup r x
explval (ArrElem x e) r s = V_Loc (k + v)
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

-- Initial store
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

-- Section 6.1

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "var", T_Sym),
                (string "if", T_Sym),
                (string "then", T_Sym),
                (string "else", T_Sym),
                (string "while", T_Sym),
                (string "do", T_Sym),
                (string "skip", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/",
                                ":=",",",";","[","]"
                                ], T_Sym)]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] | If Expr (Cmd,Cmd)
         | While Expr Cmd | Skip
data Decl = VarDecl Ide | ArrDecl Ide Numeral
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr

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
        lit "if" `x_seq` expr `seqp`
                (lit "then" `x_seq` cmd `seqp` (lit "else" `x_seq` cmd ))
                        `using` uncurry If `alt`
        lit "while" `x_seq` expr `seqp` (lit "do" `x_seq` cmd)
                        `using` uncurry While `alt`
        lit "skip" `using` const Skip `alt`
        expr `seqp` lit ":=" `x_seq` expr `using` uncurry Assign
seqcmd= cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)

expr = expr'' `seqp` rexpr' `using` uncurry (flip id)
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
test1 = exec "begin var n,f; n:=5; f:=1; while n>0 do begin f:=f*n; n:=n-1 end; result:=f end"