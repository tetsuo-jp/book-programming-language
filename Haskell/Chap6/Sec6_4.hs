module Sec6_4 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain

cmdexec :: Cmd -> Env -> Cont -> Cont
cmdexec (Assign e e') r p s     -- p.122
    = explexec e r p' s
      where
      p' (V_Loc k) s = expexec e' r p'' s
             where
             p'' v (l,t) = p (l, update t k v)
cmdexec (Block ds cs) r p s@(l,_) =
        foldr (flip cmdexec r') p' cs s'
        where
        (r',s') = foldl (flip vardecl) (r,s) ds
        p' (_,t) = p (l,t)
cmdexec (If e (c,c')) r p s
        = expexec e r p' s
        where
        p' (V_Bool v) s =
            if v then cmdexec c  r p s
                 else cmdexec c' r p s
cmdexec c@(While e c') r p s
        = expexec e r p' s
        where
        p' (V_Bool v) s =
            if v then let r' = update r "*BREAK*" (V_Cont p)
                      in cmdexec c' r' (cmdexec c r p) s
                 else p s
cmdexec Skip r p s = p s
cmdexec (Output e) r p s = expexec e r p' s
    where
    p' v s = v : p s -- expvalをexpexecで書き換え@Sec6_4
cmdexec Break r p s = p' s
        where V_Cont p' = lookup r "*BREAK*" -- p.121

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
                              where
                              (l',s') = alloc s (numval n)

-- Expression (R-Value): p.123, fig.6.5
expexec :: Expr -> Env -> E_Cont -> Cont
expexec (Num n) r q = q (V_Int (numval n))
expexec e@(Var x) r q
    = explexec e r q'
      where
      q' (V_Loc k) s@(_,t) = q (lookup t k) s
expexec (ArrElem x e) r q
    = expexec e r q'
      where
      q' (V_Int v) s@(_,t) = q (lookup t (k + v)) s
          where
            V_Loc k = lookup r x
expexec (Bexpr o e e') r q
    = expexec e r q'
      where
      q' (V_Int v) = expexec e' r q''
          where
          q'' (V_Int v') = q (V_Int (binopr o v v'))
expexec (AssExpr e e') r q
    = explexec e r q'
      where
      q' (V_Loc k) = expexec e' r q''
          where
          q'' v (l,t) = q v (l, update t k v)
expexec (SeqExpr es) r q
    = foldl1 (flip (.) ((.) const) . (.))
                   (map (flip expexec r) es) q
expexec (Rexpr o e e') r q      -- Sec6_4
    = expexec e r q'
      where
      q' (V_Int v) = expexec e' r q''
          where
          q'' (V_Int v') = q (V_Bool (relopr o v v'))

-- Expression (L-Value)
explexec :: Expr -> Env -> E_Cont -> Cont
explexec (Var x) r q = q (V_Loc k)
      where V_Loc k = lookup r x
explexec (ArrElem x e) r q      -- Sec6_4
    = expexec e r q'
      where
        q' (V_Int v) = q (V_Loc (k + v))
          where
          V_Loc k = lookup r x
explexec e r q       = error ("in explexec: "++show e)


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
s0 = (0, t0)

-- Initial environment
r0 :: Env
r0 = none

-- Initial continuation
stop :: Cont
stop s = []

-- Execution function
exec c = cmdexec (parse c) r0 stop s0 -- p.118

-- Interactive execution
-- exec' = interact (show' . exec)

-- Section 6.2
-- Fig.6.4: Continuation semantics

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
                (string "output", T_Sym),
                (string "break", T_Sym), -- Sec6_3
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/",
                                ":=",",",";","[","]"
                                ], T_Sym)]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] | If Expr (Cmd,Cmd)
         | While Expr Cmd | Skip | Output Expr | Break -- p.116, p.120
           deriving Show
data Decl = VarDecl Ide | ArrDecl Ide Numeral
            deriving Show
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
          | AssExpr Expr Expr | SeqExpr [Expr] -- p.123に出てきている
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
        lit "if" `x_seq` expr `seqp`
                (lit "then" `x_seq` cmd `seqp` (lit "else" `x_seq` cmd) )
                        `using` uncurry If `alt`
        lit "while" `x_seq` expr `seqp` (lit "do" `x_seq` cmd)
                        `using` uncurry While `alt`
        lit "skip" `using` const Skip `alt`
        lit "break" `using` const Break `alt`
        lit "output" `x_seq` expr `using` Output `alt`
        expr `seqp` lit ":=" `x_seq` expr `using` uncurry Assign
seqcmd= cmd `seqp` many (lit ";" `x_seq` cmd) `using` uncurry (:)

expr = expr'' `seqp` rexpr' `using` uncurry (flip id) `alt`
       expr'' `seqp` expr''' `using` uncurry (flip id)
expr'''=lit ":=" `x_seq` expr `using` flip AssExpr `alt`
        succeed id
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
test1 = parse"begin var n; n:=3; output n; output (n:=5) end"
test2 = exec "begin var n; n:=3; output n; output (n:=5) end"
test3 = parse"begin var n; if (n:=3)==3 then output 1 else output 2 end"
test4 = exec "begin var n; if (n:=3)==3 then output 1 else output 2 end"
test5 = parse"begin var n; n:=5; while n>0 do n:=n-1; output n end"
test6 = exec "begin var n; n:=5; while n>0 do begin n:=n-1; output n end end"
test7 = parse"begin var n; output (n:=5; n:=n-1) end"
test8 = exec "begin var n; output (n:=5; n:=n-1) end"
test9 = parse"begin var a[3],n; a[n:=1]:=99; output a[n:=1] end"
test10= exec "begin var a[3],n; a[n:=1]:=99; output a[n:=1] end"
test11= parse"begin var a[3],n; a[n:=1]:=99; output a[(a[2]:=100;2)] end"
test12= exec "begin var a[3],n; a[n:=1]:=99; output a[(a[2]:=100;2)] end"