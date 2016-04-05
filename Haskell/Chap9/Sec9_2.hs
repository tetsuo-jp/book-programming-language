module Sec9_2 where
import Prelude hiding (lex,fail)
import Data.Char

type Code = [Instr]                       -- p.183
type State = (Stack, Store)               -- p.184
type Stack = [Val]                        -- p.184
type Store = Assoc Ide Val                -- p.???
type Numeral = [Char]                     -- p.???
type Ide = [Char]                         -- p.???
data Val = V_Int Int | V_Bool Bool deriving Show -- p.???
data BinOpr = Plus | Minus | Times | Over -- p.185
              deriving Show
data RelOpr = Equal | NotEqual | Greater  -- p.185
            | GreaterEq | Less | LessEq
              deriving Show
data Instr = PushNum Int | PushVar Ide | PopVar Ide -- p.186
           | Bopr BinOpr | Ropr RelOpr
           | Jump Code | JumpFalse Code
           | Stop
             deriving Show
type Prog = Expr
data Expr = Num Numeral         -- p.187, p.194
          | Pexpr Expr Numeral
          | Mexpr Expr Numeral

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
-- parse = syn . lex               -- p.188, p.205

type Token t = (t, [Char])      -- p.199, p.201

lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

strip :: Eq t => t -> [Token t] -> [Token t]
strip t = filter ((/= t) . fst)

-- syn :: [Token t] -> Prog        -- p.199
syn = fst . head . prog

prog :: Parser Char Expr
prog = expr

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

expr :: Parser Char Expr        -- p.196
expr = (numeral `using` Num) `seqp` expr'
       `using` uncurry (foldl (flip id))

expr' :: Parser Char [Expr->Expr] -- p.196
expr' =   (literal '+' `x_seq` numeral
          `using` flip Pexpr)
    `seqp` expr' `using` uncurry (:)
    `alt` (literal '-' `x_seq` numeral
          `using` flip Mexpr)
    `seqp` expr' `using` uncurry (:)
    `alt` succeed []

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
         deriving Eq

tok :: Parser Char [Char] -> t -> Parser Char (Token t) -- p.202
tok p t xs = [((t,v),xs') | (v,xs') <- p xs]

lexime :: [(Parser Char [Char], t)] -> Parser Char [Token t] -- p.203
lexime = many . foldr (alt . uncurry tok) fail

lexer = lexime [ (some (satisfy isSpace), T_Junk), -- p.203
                 (number, T_Num),
                 (anyof string ["+", "-"], T_Sym) ]

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


-- テスト
test3 = plus "+"                -- p.191
test4 = plus "-+"               -- p.191

plusminus = seqp (literal '+') (literal '-')

test5 = plusminus "+-*"         -- p.194
test6 = plusminus "-+"