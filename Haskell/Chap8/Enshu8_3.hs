module Enshu8_3 where
    -- 値呼び, Extended to Enshu8_2_b, Sec8_1_b, Sec8_1_a, Enshu6_7

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
        r'' = update r "*EXIT*" (V_Cont p) -- p.130 演習問題6.5
        (r',s') = foldl (flip vardecl) (r'',s) ds
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
                          p'' = cmdexec c r p
                          r'' = update r' "*CONTINUE*" (V_Cont p'')
                      in cmdexec c' r'' p'' s
                 else p s
cmdexec Skip r p s = p s
cmdexec (Output e) r p s = expexec e r p' s
    where
    p' v s = v : p s -- expvalをexpexecで書き換え@Sec6_4
cmdexec Break r p s = p' s
        where V_Cont p' = lookup r "*BREAK*" -- p.121
cmdexec (Call x [e]) r p s                   -- p.178, p.164
    = expexec e r q s
    where
    V_Proc u = lookup r x
    q v = u [v] p
    -- q (V_Int v) = u [V_Int v] p
-- cmdexec (Call x [e]) r p s                   -- p.164
--     = u [expval e r s] p s
--     where
--     V_Proc u = lookup r x
-- cmdexec (Call x _) r p s = u [] p s
--         where
--         V_Proc u = lookup r x
cmdexec Exit r p s = p' s
        where V_Cont p' = lookup r "*EXIT*" -- p.130 演習問題6.5
cmdexec Continue r p s = p' s
        where V_Cont p' = lookup r "*CONTINUE*" -- p.130 演習問題6.6
cmdexec (Return e) r p s = expexec e r q s
    where q v (l,t) = p (l,update t l' v)
              where
              V_Loc l' = lookup r "*RETURN*"

-- Variable Declaration
vardecl :: Decl -> (Env,State) -> (Env,State)
vardecl (VarDecl x) (r,s) = (update r x (V_Loc l'),s')
                            where
                            (l',s') = alloc s 1
vardecl (ArrDecl x n) (r,s) = (update r x (V_Loc l'),s')
                              where
                              (l',s') = alloc s (numval n)
vardecl (ProcDecl x [y] c) (r,s) = (r',s) -- p.164, p.165
    where
    r' = update r x (V_Proc u)
    u [v] p s = cmdexec c (update r' y (V_Loc l)) p s'
                where
                (l, (l', t')) = alloc s 1
                s' = (l', update t' l v)
-- vardecl (ProcDecl x _ c) (r,s) = (r', s) -- p.125-126
--         where
--         r' = update r x (V_Proc u)
--         u _ = cmdexec c r'
vardecl (FuncDecl x _ c) (r,s)
    = (r'', s') -- p.125-126, p.130 演習問題6.7
        where
        r' = update r x (V_Proc h)
        (l',s') = alloc s 1
        r'' = update r' "*RETURN*" (V_Loc l')
        h _ = cmdexec c r''

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
expexec (Apply f _) r q = h [] p -- p.126
        where
        V_Proc h = lookup r f
        p (l,t) = q v (l,t)
              where
              V_Loc v' = lookup r "*RETURN*"
              v = lookup t v'
expexec (Ref e) r q = explexec e r q   -- p.178-179
expexec (Deref e) r q = expexec e r q' -- p.178-179
        where
        q' (V_Loc k) s@(_,t) = q (lookup t k) s

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
explexec (Ref e) r q = undefined -- p.109
explexec (Deref e) r q = expexec e r q -- p.178-179
explexec e r q       = error ("in explexec: "++show e)

-- Expression (R-value), p.99, p.102
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

-- -- Expression (L-value), p.102
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
          t' = foldl (flip flip undefined . update) t [l..l+n-1] -- 修正

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
                (string "func", T_Sym), -- Sec6_5
                (string "proc", T_Sym), -- Sec6_5
                (string "call", T_Sym), -- Sec6_5
                (string "exit", T_Sym), -- p.130 演習問題6.5
                (string "continue", T_Sym), -- p.130 演習問題6.6
                (string "return", T_Sym),   -- p.130 演習問題6.7
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/",
                                ":=",",",";","[","]",
                                "=", -- Sec6_5
                                "&" -- p.108, p.178-179
                                ], T_Sym)]

-- Syntactic domains
data Cmd = Assign Expr Expr | Block [Decl] [Cmd] | If Expr (Cmd,Cmd)
         | While Expr Cmd | Skip | Output Expr | Break  -- p.116, p.120
         | Call Ide [Expr]                              -- p.126
         | Exit                                         -- p.130 演習問題6.5
         | Continue                                     -- p.130 演習問題6.6
         | Return Expr                                  -- p.130 演習問題6.7
           deriving Show
data Decl = VarDecl Ide | ArrDecl Ide Numeral
          | ProcDecl Ide [Ide] Cmd -- p.125
          | FuncDecl Ide [Ide] Cmd -- p.130 演習問題6.7
            deriving Show
data Expr = Num Numeral | Var Ide | ArrElem Ide Expr | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
          | AssExpr Expr Expr | SeqExpr [Expr] -- p.123に出てきている
          | Apply Ide [Expr]                   -- p.126
          | Ref Expr | Deref Expr -- p.108, p.178-179
            deriving Show

-- Syntax analysis
decls = many (decl `seq_x` lit ";") `using` concat
decl = lit "var" `x_seq`
        (decl' `seqp` many (lit "," `x_seq` decl') `using` uncurry (:))  `alt`
       (lit "func" `x_seq` kind T_Ide `seq_x`
             lit "(" `seq_x` lit ")" `seq_x` lit "=") `seqp`
          cmd                   -- p.130 演習問題6.7
                        `using` (\(f,c) -> [FuncDecl f [] c]) `alt`
       ((lit "proc" `x_seq` kind T_Ide `seq_x` lit "(")
             `seqp` kind T_Ide `seq_x` lit ")" `seq_x` lit "=") `seqp`
          cmd
                        `using` (\((f,a),c) -> [ProcDecl f [a] c])
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
        expr `seqp` lit ":=" `x_seq` expr `using` uncurry Assign `alt`
        ((lit "call" `x_seq` kind T_Ide `seq_x` lit "(")
                 `seqp` expr `seq_x` lit ")"
            `using` \(p,e) -> Call p [e]) `alt`
        lit "exit" `using` const Exit `alt`   -- p.130 演習問題6.5
        lit "continue" `using` const Continue `alt` -- p.130 演習問題6.6
        lit "return" `x_seq` expr `using` Return -- p.130 演習問題6.7
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
expr''= term `seqp` expr' `using` uncurry (foldl (flip id)) `alt`
        kind T_Ide `seq_x` lit "(" `seq_x` lit ")"
                 `using` \f -> Apply f []
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
                (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
                (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
        kind T_Ide `seqp` indexed `using` uncurry (flip id) `alt`
        lit "&" `x_seq` factor `using` Ref `alt`   -- p.178-179
        lit "*" `x_seq` factor `using` Deref `alt` -- p.178-179
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
-- test1 = parse "begin var x; func f() = begin x := x+1; return x end; x := 0; output f(); output f() end" -- p.131
-- test2 = exec  "begin var x; func f() = begin x := x+1; return x end; x := 0; output f(); output f() end" -- p.131

-- test3 = cmd $ lex "begin var x; func f() = begin x := x+1; return x end; x := 0; output f(); output f() end"
-- test4 = cmd $ lex "return x"
-- test5 = cmd $ lex "begin x := x+1; return x end"
-- test6 = exec "begin var x; func f() = begin x := x+1; return x end; x := 0; output f() end"

-- p.163
testprog1 = "\
             \begin var z;\
             \    proc inc(x) = x := x + 1;\
             \    z := 0; call inc(z); output z \
             \end\
             \"              -- "

test10 = parse testprog1
test11 = exec testprog1

-- p.165
testprog2 = "\
             \begin var x, z;\
             \    proc sum(x) =\
             \        while x > 0 do\
             \            begin z := z + x; x := x - 1 end;\
             \    x := 1; z := 0; call sum(x+3); output x; output z \
             \end\
             \"              -- "
test12 = parse testprog2
test13 = exec testprog2         -- [1,10]

test20 = parse "begin var x; x:=8; output &(*(&x)) end"
test21 = exec "begin var x; x:=8; output &(*(&x)) end"

testprog3 = "\
             \begin\
             \    var z;\
             \    proc inc(x) = *x := *x + 1;\
             \    z := 0; call inc(&z); output z \
             \end\
             \"                 -- "

test31 = parse testprog3
test32 = exec testprog3
