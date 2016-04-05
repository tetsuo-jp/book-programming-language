module Enshu5_6 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain


-- Enshu 5.4

cmdexec :: Cmd -> Env -> State -> State
cmdexec (Assign x e) r s@(l,t)
    | err = error "Assigned to constants"
    where err = case lookup r x of
                  V_Loc _ -> False
                  _       -> True
cmdexec (Assign x e) r s@(l,t) = (l, update t k (expval e r s))
                               where
                               V_Loc k = lookup r x
-- cmdexec (Assign x e) r s@(l,t) = (l, update t k (expval e r s))
--                 where
--                 V_Loc k = lookup r x
cmdexec (Block ds cs) r s@(l,t) = if length t >= 0 then (l, t') else undefined
        where
        (r',s') = foldl (flip vardecl) (r,s) ds
        (_, t') = foldl (flip (flip cmdexec r')) s' cs

vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x e) (r,s) = (r',s'')
                            where
                            (l',s'@(l'',t)) = alloc s 1
                            r' = update r x (V_Loc l')
                            s'' = (l'', update t l' (expval e r s'))
vardecl (ConDecl x n) (r,s) = (r',s)
                            where
                            r' = update r x (V_Int (numval n))

expval :: Expr -> Env -> State -> Val
expval (Num n) r s = V_Int (numval n)
expval (Var x) r (_,t) = case lookup r x of -- p.108, Enshi 5.4
                           V_Loc k -> lookup t k
                           V_Int n -> V_Int n
expval (Bexpr o e e') r s = V_Int (binopr o v v')
                            where
                            V_Int v = expval e r s
                            V_Int v' = expval e' r s
expval (If e (e',e'')) r s | v         = expval e' r s
                           | otherwise = expval e'' r s
    where V_Bool v = expval e r s
expval (Ref e) r s = explval e r s -- p.108
expval (Deref e) r s@(_,t) = lookup t k -- p.108-109
                             where
                             V_Loc k = expval e r s

-- Expression (L-value)
explval :: Expr -> Env -> State -> Val
explval (Num n) r s = undefined
explval (Var x) r s = lookup r x
explval (Bexpr o e e') r s = undefined
-- explval (Rexpr o e e') r s = undefined
-- explval (AssExpr x e) r s = undefined
explval (Ref e) r s = undefined -- p.109
explval (Deref e) r s = v       -- p.109
                      where
                      v@(V_Loc _) = expval e r s


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

-- Section 5.3
-- Fig.5.7: Variable declarations and storage allocation

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "begin", T_Sym),
                (string "end", T_Sym),
                (string "var", T_Sym),
                (string "const", T_Sym),
                (ident, T_Ide),
                (anyof string ["=","(",")","+","-","*","/",":=",",",";",
                              "&" -- p.108
                              ], T_Sym) ]

-- Syntactic domains
data Cmd = Assign Ide Expr | Block [Decl] [Cmd]
           deriving Show
data Decl = VarDecl Ide Expr | ConDecl Ide Numeral
            deriving Show
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr | If Expr (Expr,Expr)
          | Ref Expr | Deref Expr -- p.108
            deriving Show

-- Syntax analysis
decls = many (decl `seq_x` lit ";") `using` concat
decl = (lit "var" `x_seq`
        (decls' `seqp` many (lit "," `x_seq` decls') `using` uncurry (:))) `alt`
       (lit "const" `x_seq`
        (cdecl `seqp` many (lit "," `x_seq` cdecl) `using` uncurry (:)))
decls' = kind T_Ide `seqp` lit "=" `x_seq` expr `using` uncurry VarDecl
cdecl = kind T_Ide `seqp` lit "=" `x_seq` kind T_Num `using` uncurry ConDecl
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
        lit "&" `x_seq` factor `using` Ref `alt`
        lit "*" `x_seq` factor `using` Deref `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"

-- Parser and Evaluator
type Prog = Cmd
prog = cmd

lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

-- Testing
test0 =  parse "begin var x=1; result := x end"

test1 = exec "begin n:=5; fac:=1; fac:=fac*n; n:=n-1 end"
test2 = exec "begin var x = 1; begin var x = x + 1; result := x * 2 end end"
test3 = parse "begin const a = 1, b = 3; a := b; result := a end"
test4 = exec "begin const a = 1, b = 3; a := b; result := a end" -- Error!
test5 = exec "begin const a = 1, b = 3; result := b+a end"

test6 = parse "begin var x=1; result := &x end"
test7 = exec "begin var x=8; result := &x end"
test8 = exec "begin var x=8; result := *(&x) end"
test9 = exec "begin var x=8; result := &(*(&x)) end"

test10= parse "begin var x=1; result := &x + 1 end"