module Enshu7_2 where

import SynLib
import ParseLib
import ScanLib
import Data.Char (ord,isAlphaNum,isAlpha,isSpace)

import Prelude hiding (seq,lex)

-- p.47
numval :: [Char] -> Int
numval = foldl (\u l -> u * 10 + l) 0 . map digval

digval :: Char -> Int
digval d = ord d - ord '0'

-- p.134, p.63 (<-- p.53)
data Expr = Num Numeral
          | Pexpr Expr Expr
          | Mexpr Expr Expr
             deriving Show

type Numeral = [Char]

-- p.134, p.57
expval :: Expr -> Int
expval (Num n)       = numval n
expval (Pexpr e1 e2) = expval e1 + expval e2
expval (Mexpr e1 e2) = expval e1 - expval e2

-- p.62
lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (anyof string ["(",")",
                                "+","-"
                                ], T_Sym) ]

-- p.64
eval :: String -> Int
eval = expval . parse

-- p.134
p_m :: Expr -> Int
p_m (Num n) = 0
p_m (Pexpr e1 e2) = p_m e1 + p_m e2 + 1
p_m (Mexpr e1 e2) = p_m e1 + p_m e2 - 1

-- p.135
data Parity = Odd | Even deriving Show

exppar :: Expr -> Parity
exppar (Num n)       = numpar n
exppar (Pexpr e1 e2) = exppar e1 +# exppar e2
exppar (Mexpr e1 e2) = exppar e1 -# exppar e2

-- p.158
numpar :: [Char] -> Parity
numpar n = if numval n `mod` 2 == 0 then Even else Odd

(+#) :: Parity -> Parity -> Parity
Odd  +# Odd  = Even
Even +# Odd  = Odd
Odd  +# Even = Odd
Even +# Even = Even

(-#) :: Parity -> Parity -> Parity
(-#) = (+#)

-- p.158
data Asn = Pos | Neg | Unknown deriving Show

expasn :: Expr -> Asn
expasn (Num n)       = if numval n >= 0 then Pos else Neg
expasn (Pexpr e1 e2) = expasn e1 +& expasn e2
expasn (Mexpr e1 e2) = expasn e1 -& expasn e2

(+&) :: Asn -> Asn -> Asn
Pos     +& Pos     = Pos
Neg     +& Neg     = Neg
Pos     +& Neg     = Unknown
Neg     +& Pos     = Unknown
Unknown +& _       = Unknown
_       +& Unknown = Unknown

(-&) :: Asn -> Asn -> Asn
Pos     -& Pos     = Unknown
Neg     -& Neg     = Unknown
Pos     -& Neg     = Pos
Neg     -& Pos     = Neg
Unknown -& _       = Unknown
_       -& Unknown = Unknown

{---------------------------------------------------------
  補助関数
 ---------------------------------------------------------}

type Prog = Expr
prog = expr

expr :: Parser (Tag,[Char]) Expr
expr =   factor `seqp`
         many ((lit "+" `x_seq` expr `using` flip Pexpr) `alt`
               (lit "-" `x_seq` expr `using` flip Mexpr) )
                  `using` uncurry (foldl (flip id))
factor = kind T_Num `using` Num `alt`
         lit "(" `x_seq` expr `seq_x` lit ")"

{---------------------------------------------------------
  テスト
 ---------------------------------------------------------}

test02 = lex "291"
test03 = lex "291 +31"          -- p.62
test04 = parse "291"
test05 = parse "291+"        -- + は無視される
test06 = parse "291 +31"        -- p.62
test07 = eval "291"
test08 = eval "291 +31"
test09 = eval "291 + 31 - 32"
test10 = eval "291 -31 - 32"    -- 292 となり計算としては誤り

test11 = p_m $ parse "291"
test12 = p_m $ parse "(1 + 2) - (3 + 5 + 3)"
test13 = p_m $ parse "1 - 2"

test21 = exppar $ parse "291"
test22 = exppar $ parse "1 + 2"
test23 = exppar $ parse "3 + 5 + 3"
test24 = exppar $ parse "(1 + 2) - (3 + 5 + 3)"

-- p.158
test25 = expasn $ parse "291"
test26 = expasn $ parse "1 + 2"
test27 = expasn $ parse "3 + 5 + 3"
test28 = expasn $ parse "(1 + 2) - (3 + 5 + 3)"
test29 = expasn $ parse "1 - (-3)"
test30 = expasn $ parse "(-1) - 3" -- Parse error!
test31 = expasn $ parse "(1-3) + (1-3)"