module Enshu5_7a where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain


-- Enshu5_7a
-- Expressions with side-effects

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign e e') r s =
                (l, update t k v)
                where
                V_Loc k = explval e r s
                (v,(l,t)) = expexec e' r s
cmdexec (Block ds cs) r s@(l,t) = (l, t')
        where
        (r',s') = foldl (flip vardecl) (r,s) ds
        (_,t') = foldl (flip (flip cmdexec r')) s' cs

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
                              where
                              (l',s') = alloc s (numval n)

-- Expression (L-value)
explval :: Expr -> Env -> State -> Val
explval (Num n) r s = undefined
explval (Var x) r s = lookup r x
explval (Bexpr o e e') r s = undefined
explval (Rexpr o e e') r s = undefined

-- Storage allocation
alloc :: State -> Int -> (Loc,State)
alloc (l,t) n = (l, (l+n,t'))
        where
        t' = foldl (flip flip undefined . update) t [l..l+n-1]
-- Expression with side-effect operators
expexec :: Expr -> Env -> State -> (Val,State)
expexec (Num n) r s = (V_Int(numval n), s)
expexec e@(Var _) r s@(_,t) = (lookup t k, s)
                              where
                              V_Loc k = explval e r s
expexec (Bexpr o e e') r s = (V_Int(binopr o v v'), s'')
                             where
                             (V_Int v,s') = expexec e r s
                             (V_Int v',s'') = expexec e' r s'
expexec (Rexpr o e e') r s = (V_Bool(relopr o v v'), s'')
                             where
                             (V_Int v,s') = expexec e r s
                             (V_Int v', s'') = expexec e' r s'
expexec (AssExpr e e') r s = (v, (l, update t k v))
                             where
                             (v,(l,t)) = expexec e' r s
                             V_Loc k = explval e r s
expexec (SeqExpr es) r s@(l,_) = foldl expexec' (undefined,s) es
                                 where
                                 expexec' (_,s) e = expexec e r s


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

-- Section 5.5
-- Expressions with side-effects

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "var", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")","+","-","*","/",":=",",",";","[","]"
                        ], T_Sym) ]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd]
         deriving Show
data Decl = VarDecl Ide | ArrDecl Ide Numeral
         deriving Show
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr
          | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
          | AssExpr Expr Expr | SeqExpr [Expr]
         deriving Show

-- Syntax analysis
cmd =	lit "begin" `x_seq` decls `seqp` seqcmd `seq_x` lit "end"
                `using` uncurry Block `alt`
        expr `seqp` lit ":=" `x_seq` expr
                `using` uncurry Assign
seqcmd= cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)
decls = many (decl `seq_x` lit ";") `using` concat
decl = lit "var" `x_seq`
        (decl' `seqp` many (lit "," `x_seq` decl') `using` uncurry (:))
decl' = kind T_Ide `seqp` lit "[" `x_seq`
                kind T_Num `seq_x` lit "]"
                        `using` uncurry ArrDecl `alt`
        kind T_Ide `using` VarDecl
expr =	expr'' `seqp` expr''' `using` uncurry (flip id)
expr'''=lit ":=" `x_seq` expr `using` flip AssExpr `alt`
        succeed id
expr'' =term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
                (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
                (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
        kind T_Ide `seqp` indexed `using` uncurry (flip id) `alt`
        lit "(" `x_seq` expr `seqp` exprs `seq_x` lit ")"
                `using` uncurry (flip id)
indexed=lit "[" `x_seq` expr `seq_x` lit "]" `using` flip ArrElem `alt`
        succeed Var
exprs = lit ";" `x_seq` expr `seqp` many (lit ";" `x_seq` expr) `using`
                (\(x,xs)-> (\y->SeqExpr (y:(x:xs))))
        `alt`
        succeed id

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
test2 = exec "begin var x; x:=1; result:=x+(x:=2) end"
test3 = exec "begin var x; result:=(x:=1; x+(x:=2)) end"
test4 = exec "begin var x; x:=1; result:=(x:=2)+x end"

test10 = lex "begin var a[10], k; k:=1; result:=(a[(k:=2)]:=k) end"
test11 = parse "begin var a[10], k; k:=1; result:=(a[(k:=2)]:=k) end"
test12 = exec "begin var a[10], k; k:=1; result:=(a[(k:=2)]:=k) end"
