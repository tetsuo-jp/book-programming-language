module Sec6_2 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain


-- Section 6.2
-- Fig.6.4: Continuation semantics

cmdexec :: Cmd -> Env -> Cont -> Cont
cmdexec (Assign e e') r p s@(l, t) =
      explexec e r p' s
      where
      p' (V_Loc k) s = expexec e' r p'' s
             where
             p'' v (l,t) = p (l, update t k v)
cmdexec (Block ds cs) r p s@(l, _) =
            foldr (flip cmdexec r') p' cs s'
            where
            (r',s') = foldl (flip vardecl) (r, s) ds
            p' (_, t) = p (l,t)
cmdexec (If e (c,c')) r p s =
            expexec e r p' s
            where
            p' (V_Bool v) s | v = cmdexec c r p s
                            | otherwise = cmdexec c' r p s
cmdexec c@(While e c') r p s =
            expexec e r p' s
            where
            p' (V_Bool v) s | v = cmdexec c' r (cmdexec c r p) s
                            | otherwise = p s
cmdexec Skip r p s = p s
cmdexec (Output e) r p s = expexec e r p' s
            where
            p' v s = v : p s

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
                              where
                              (l',s') = alloc s (numval n)

-- Expression (R-value)
expexec :: Expr -> Env -> E_Cont -> Cont
expexec (Num n) r q = q (V_Int (numval n))
expexec e@(Var x) r q = explexec e r q'
                         where
                         q' (V_Loc k) s@(_,t) = q (lookup t k) s
expexec (Bexpr o e e') r q = expexec e r q'
                            where
                            q' (V_Int v) = expexec e' r q''
                                where
                                q'' (V_Int v') = q (V_Int (binopr o v v'))
expexec (AssExpr e e') r q = explexec e r q'
                            where
                            q' (V_Loc k) = expexec e' r q''
                                where
                                q'' v (l,t) = q v (l, update t k v)
expexec (SeqExpr es) r q = foldl1 (flip (.) ((.) const) . (.))
                                  (map (flip expexec r) es) q

explexec :: Expr -> Env -> E_Cont -> Cont
explexec (Var x) r q
    = q (V_Loc k)
      where
      V_Loc k = lookup r x
explexec e r q = undefined

-- -- Expression (L-value)
-- explval :: Expr -> Env -> State -> Val
-- explval (Num n) r s = undefined
-- explval (Var x) r s = lookup r x
-- explval (ArrElem x e) r s = V_Loc (k + v)
--                             where
--                             V_Loc k = lookup r x
--                             V_Int v = expval e r s
-- explval (Bexpr o e e') r s = undefined
-- explval (Rexpr o e e') r s = undefined

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
exec c = cmdexec (parse c) r0 stop s0

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
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/",
                                ":=",",",";","[","]"
                                ], T_Sym)]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] | If Expr (Cmd,Cmd)
         | While Expr Cmd | Skip | Output Expr
           deriving Show
data Decl = VarDecl Ide | ArrDecl Ide Numeral
           deriving Show
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
          | AssExpr Expr Expr | SeqExpr [Expr] -- cf. p.104
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
        lit "output" `x_seq` expr `using` Output `alt`
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
test1 = exec "begin var x; x := 1; output x end"
test2 = exec "begin var x, y; x := 2; y := 3; output x * y end"