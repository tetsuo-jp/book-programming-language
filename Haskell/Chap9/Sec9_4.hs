module Sec9_4 where             -- Extended to Sec9_3, Sec9_2
import Prelude hiding (lex,fail)
import Data.Char

type Code = [Instr]                       -- p.183
type State = (Stack, Store)               -- p.184
type Stack = [Val]                        -- p.184
data Instr = PushNum Int | PushVar Ide | PopVar Ide -- p.186
           | Bopr BinOpr | Ropr RelOpr
           | Jump Code | JumpFalse Code
           | Stop
             deriving Show

data Cmd = Assign Ide Expr | Seq [Cmd] -- p.210 (図9.15), p.206, 図9.12
         | If Expr (Cmd,Cmd) | While Expr Cmd | Skip
           deriving Show
data Expr = Num Numeral | Var Ide -- p.208, p.206, 図9.12, (p.187, p.194)
          | Bexpr BinOpr Expr Expr
          | Rexpr RelOpr Expr Expr
            deriving Show
type Numeral = [Char] -- p.206, 図9.12
type Ide = [Char] -- p.206, 図9.12
data BinOpr = Plus | Minus | Times | Over -- p.206, 図9.12 (p.185)
              deriving Show
data RelOpr = Equal | NotEqual | Greater -- p.206, 図9.12 (p.185)
            | GreaterEq | Less | LessEq
              deriving Show

data Val = V_Int Int | V_Bool Bool -- p.206, 図9.12
         deriving Show
type Store = Assoc Ide Val

cmdexec :: Cmd -> Store -> Store -- p.206, 図9.12
cmdexec (Assign x e) s = update s x (expval e s)
cmdexec (Seq cs) t = foldl (flip cmdexec) t cs
cmdexec (If e (c,c')) t
   | v = cmdexec c t
   | otherwise = cmdexec c' t
   where
   V_Bool v = expval e t
cmdexec c@(While e c') t
   | v = cmdexec c (cmdexec c' t)
   | otherwise = t
   where
   V_Bool v = expval e t
cmdexec Skip s = s

numval :: Numeral -> Int        -- 練習問題3.1
numval ns = numval' (reverse ns)
  where numval' (n:ns) = digval n + numval' ns * 10
        numval' []     = 0

-- digval :: Digit -> Int          -- 練習問題3.1
digval d | '0' <= d && d <= '9' = ord d - ord '0'

expval :: Expr -> Store -> Val -- p.206, 図9.12
expval (Num n) t = V_Int (numval n)
expval (Var x) t = aLookup t x
expval (Bexpr o e e') t
       = V_Int (binopr o v v')
       where
       V_Int v  = expval e  t
       V_Int v' = expval e' t
expval (Rexpr o e e') t
       = V_Bool (relopr o v v')
       where
       V_Int v  = expval e  t
       V_Int v' = expval e' t

-- Implementation in terms of lists of pairs
type Assoc a b = [(a,b)]
none = []
assoc d = [(undefined,d)]
aLookup [] x = undefined
aLookup [(_,d)] x = d
aLookup ((y,v):h) x | x==y = v
                   | otherwise = aLookup h x
update h x v = (x,v) : h

-- Arithmetic operations
binopr :: BinOpr -> Int -> Int -> Int
binopr Plus = (+)
binopr Minus = (-)
binopr Times = (*)
binopr Over = div

-- Relational operations
relopr :: RelOpr -> Int -> Int -> Bool
relopr Equal = (==)
relopr NotEqual = (/=)
relopr Greater = (>)
relopr GreaterEq = (>=)
relopr Less = (<)
relopr LessEq = (<=)

exec :: Code -> State -> State
exec (Jump is':_)   s        = exec is' s
exec (Stop:_)       s        = s
exec (PushNum n:is) (st,t)   = exec is (V_Int n:st,t)
exec (PushVar x:is) (st, t)  = exec is (aLookup t x:st,t)
exec (PopVar x:is)  (v:st,t) = exec is (st, update t x v)
exec (Bopr o:is)    (V_Int v':V_Int v:st,t)
                             = exec is (V_Int (binopr o v v'): st,t)
exec (Ropr o:is)    (V_Int v':V_Int v:st,t)
                             = exec is (V_Bool (relopr o v v'):st,t)
exec (JumpFalse is':is) (V_Bool v:st, t)
                 | v         = exec is  (st,t)
                 | otherwise = exec is' (st,t)

-- テスト
test1 =
    [PushVar "a",
     PushVar "b",
     PushVar "c",
     Bopr Times,
     Bopr Plus,
     PopVar "result",
     Stop]
test2 =
    [PushVar "x",
     PushVar "y",
     Ropr Less,
     JumpFalse [Stop],
     PushVar "y",
     PopVar "x",
     Stop]

-- parse :: [Char] -> Prog         -- p.188
parse :: [Char] -> Cmd
parse = syn . lex               -- p.188, p.205

type Token t = (t, [Char])      -- p.199, p.201

lex :: [Char] -> [Token Tag]    -- p.204
lex = (strip T_Junk) . fst . head . lexer

strip :: Eq t => t -> [Token t] -> [Token t]
strip t = filter ((/= t) . fst)

-- syn :: [Token t] -> Prog        -- p.199
syn = fst . head . prog

-- prog :: Parser Char Expr
-- prog = expr

-- type Parser a b = [a] -> [(b, [a])]

-- succeed :: b -> Parser a b
-- succeed v xs = [(v, xs)]

-- fail :: Parser a b
-- fail xs = []

satisfy :: (a -> Bool) -> Parser a a
satisfy q []     = fail []
satisfy q (x:xs)
     | q x       = succeed x xs
     | otherwise = fail xs

-- literal :: Eq a => a -> Parser a a
-- literal x = satisfy (== x)

plus = literal '+'

-- seqp :: Parser a b -> Parser a c -> Parser a (b,c)
-- seqp p1 p2 xs = [((v,v'),xs'') | (v,xs') <- p1 xs, (v',xs'') <- p2 xs']

-- alt :: Parser a b -> Parser a b -> Parser a b
-- alt p1 p2 xs = p1 xs ++ p2 xs

-- using :: Parser a b -> (b -> c) -> Parser a c
-- using p f xs = [(f v, xs') | (v, xs') <- p xs]

-- infixr 6 `x_seq`, `seq_x`
-- x_seq :: Parser a b -> Parser a c -> Parser a c
-- x_seq p1 p2 xs = [(v,xs'') | (_,xs') <- p1 xs, (v,xs'') <- p2 xs']
-- seq_x :: Parser a b -> Parser a c -> Parser a c
-- seq_x p1 p2 xs = [(v,xs'') | (v,xs') <- p1 xs, (_,xs'') <- p2 xs']

{-
-- うまくいかない
expr :: Parser Char Expr        -- p.195
expr   =   numeral `using` Num
     `alt` expr `seqp` literal '+' `x_seq` numeral
                                   `using` uncurry Pexpr
     `alt` expr `seqp` literal '-' `x_seq` numeral
                                   `using` uncurry Mexpr
-}

numeral :: Parser Char Numeral
numeral = number                -- p.???

-- expr :: Parser Char Expr        -- p.196
-- expr = (numeral `using` Num) `seqp` expr'
--        `using` uncurry (foldl (flip id))

-- expr' :: Parser Char [Expr->Expr] -- p.196
-- expr' =   (literal '+' `x_seq` numeral
--           `using` flip (Bexpr Plus))
--     `seqp` expr' `using` uncurry (:)
--     `alt` (literal '-' `x_seq` numeral
--           `using` flip (Bexpr Minus))
--     `seqp` expr' `using` uncurry (:)
--     `alt` succeed []

-- many :: Parser a b -> Parser a [b]
-- many p   =   p `seqp` many p `using` uncurry (:)
--        `alt` succeed []

-- expr' :: Parser Char [Expr->Expr]        -- p.196
-- expr' = many ((literal '+' `using` Pexpr -- p.197
--                `alt` literal '-' `using` Mexpr)
--               `seqp` numeral `using` uncurry flip)

-- some :: Parser a b -> Parser a [b]
-- some p = (p `seqp` many p) `using` uncurry


-- p.198, 図9.9
type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v xs = [(v,xs)]

fail :: Parser a b
fail xs = []

literal :: Eq a => a -> Parser a a
literal x = satisfy (==x)

infixr 4 `alt`
alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) xs = p1 xs ++ p2 xs

infixr 6 `seqp`
seqp :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `seqp` p2) xs =
    [((v,v'),xs'') | (v,xs') <- p1 xs, (v',xs'') <- p2 xs']

infixl 5 `using`
using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) xs = [(f v,xs') | (v,xs') <- p xs]

many :: Parser a b -> Parser a [b]
many p = p `seqp` many p `using` uncurry (:) `alt` succeed []

some :: Parser a b -> Parser a [b]
some p = p `seqp` many p `using` uncurry (:)

infixr 6 `x_seq`, `seq_x`
x_seq :: Parser a b -> Parser a c -> Parser a c
p1 `x_seq` p2 = p1 `seqp` p2 `using` snd
seq_x :: Parser a b -> Parser a c -> Parser a b
p1 `seq_x` p2 = p1 `seqp` p2 `using` fst

anyof :: (a -> Parser b c) -> [a] -> Parser b c
anyof r = foldr (alt . r) fail

number :: Parser Char [Char]    -- p.200
number = some (satisfy isDigit)

ident :: Parser Char [Char]     -- p.201
ident = satisfy isAlpha `seqp` many (satisfy isAlpha)
        `using` uncurry (:)

string :: Eq a => [a] -> Parser a [a] -- p.201
string []     = succeed []
string (x:xs) = literal x `seqp` string xs
                `using` uncurry (:)

data Tag = T_Num | T_Ide | T_Sym | T_Junk -- p.201
         deriving (Eq,Show)

tok :: Parser Char [Char] -> t -> Parser Char (Token t) -- p.202
tok p t xs = [((t,v),xs') | (v,xs') <- p xs]

lexime :: [(Parser Char [Char], t)] -> Parser Char [Token t] -- p.203
lexime = many . foldr (alt . uncurry tok) fail

-- lexer = lexime [ (some (satisfy isSpace), T_Junk), -- p.203
--                  (number, T_Num),
--                  (anyof string ["+", "-"], T_Sym) ]

{-
-- p.204 図9.10
type Token t = (t,[Char])

infix 7 `tok`
tok :: Parser Char [Char] -> t -> Parser Char (Token t)
(p `tok` t) xs = [((t,v),xs') | (v,xs') <- p xs]

lexime :: [(Parser Char [Char], t)] -> Parser Char [Token t]
lexime = many . foldr (alt . uncurry tok) fail

trip :: Eq t => t -> [Token t] -> [Token t]
trip t = filter ((/= t) . fst)

ind :: Eq t => t -> Parser (Token t) [Char]
ind t = satisfy ((== t) . fst) `using` snd

number :: Parser Char [Char]
number = some (satisfy isDigit)

string :: Eq a => [a] -> Parser a [a]
string []     = succeed []
string (x:xs) = (literal x `seqp` string xs) `using` uncurry (:)

lex :: [Char] -> [Token Tag]    -- p.204
lex = (strip T_Junk) . fst . head . lexer
-}

kind :: Eq t => t -> Parser (Token t) [Char] -- ScanLibからコピー
kind t = satisfy ((== t) . fst) `using` snd

-- 図9.13
lit:: [Char] -> Parser (Token Tag) [Char]
lit xs = literal (T_Sym,xs) `using` snd

lexer :: Parser Char [Token Tag] -- p.203
lexer = lexime [(some (satisfy isSpace), T_Junk),
         (number, T_Num),
         (string "begin", T_Sym), (string "end", T_Sym),
         (string "if", T_Sym), (string "then", T_Sym),
         (string "else", T_Sym), (string "while", T_Sym),
         (string "do", T_Sym), (string "skip", T_Sym),
         (ident, T_Ide),
         (anyof string ["(",")","+","-","*","/",":=",";",
                        "==","/==",">=",">","<=","<"], T_Sym)
         ]
prog = cmd
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
   `alt` lit ">"  `x_seq` expr' `using` flip (Rexpr Greater)
   `alt` lit ">=" `x_seq` expr' `using` flip (Rexpr GreaterEq)
   `alt` lit "<"  `x_seq` expr' `using` flip (Rexpr Less)
   `alt` lit "<=" `x_seq` expr' `using` flip (Rexpr LessEq)
   `alt` succeed id
expr' = term `seqp` expr'' `using` uncurry (foldl (flip id))
expr''= many ((lit "+" `x_seq` term `using` flip (Bexpr Plus))
        `alt` (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term= factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times))
        `alt` (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =   kind T_Num `using` Num
     `alt` kind T_Ide `using` Var
     `alt` lit "(" `x_seq` expr `seq_x` lit ")"

-- 図9.14, p.208
genexp :: Expr -> Code
genexp (Num n) = [PushNum (numval n)]
genexp (Var x) = [PushVar x]
genexp (Bexpr o e e')
       = genexp e ++ genexp e' ++ [Bopr o]
genexp (Rexpr o e e')
       = genexp e ++ genexp e' ++ [Ropr o]

-- 図9.15, p.210
gencmd :: Cmd -> Code -> Code
gencmd (Assign x e) is
       = genexp e ++ [PopVar x] ++ is
gencmd (Seq cs) is = foldr gencmd is cs
gencmd (If e (c,c')) is
       = genexp e ++ [JumpFalse is']
                  ++ gencmd c [Jump is] ++ is'
         where
         is' = gencmd c' is
gencmd (While e c) is = is'
       where
       is' = genexp e ++ [JumpFalse is]
                      ++ gencmd c [Jump is'] ++ is
gencmd Skip is = is

-- p.210
compile s = gencmd (parse s) [Stop]

st0 :: Stack
st0 = []

t0 :: Store
t0 = none

runcode is =
     aLookup (snd (exec is (st0,t0))) "result"

-- テスト
testprog1 = "\
             \begin\
             \    if 1 > 0 then v := 3 else v := 1;\
             \    while 1 < 0 do skip;\
             \    result := v \
             \end\
             \"                 -- "
test3 = parse testprog1
test4 = length (compile testprog1)
test5 = runcode $ compile testprog1
test6 = runcode $ compile "result := 1"

test7 = parse "result := v"
test8 = parse "begin v := 1; result := v end"