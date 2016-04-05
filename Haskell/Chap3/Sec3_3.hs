module Sec3_3 where

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

-- p.63 (<-- p.53)
data Expr = Num [Char]
          | Pexpr Expr Expr
          | Mexpr Expr Expr
             deriving Show

-- p.57
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