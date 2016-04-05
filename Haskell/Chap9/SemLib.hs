module SemLib where

import Data.Char(ord)
import Prelude hiding (lookup)

-- Semantic domains and functions

-- Standard types
type Numeral = [Char]
type Ide = [Char]
data BinOpr = Plus | Minus | Times | Over deriving Show
data RelOpr = Equal | NotEqual | Greater | GreaterEq | Less | LessEq
            deriving Show

-- Standard semantic functions
numval :: Numeral -> Int
numval = foldl1 (*+) . map digval
         where
         a *+ b = a * 10 + b

digval :: Char -> Int
digval a = ord a - ord '0'

-- Abstract type 'Assoc' defined in Chapter 2
none :: Assoc a b
assoc :: b -> Assoc a b
lookup :: Eq a => Assoc a b -> a -> b
update :: Eq a => Assoc a b -> a -> b -> Assoc a b

-- Implementation in terms of functions
--type Assoc a b = a -> b
--none x = undefined
--assoc d x = d
--lookup h x = h x
--update h x v y | x==y = v
--               | otherwise = lookup h y

-- Implementation in terms of lists of pairs
type Assoc a b = [(a,b)]
none = []
assoc d = [(undefined,d)]
lookup [] x = undefined
lookup [(_,d)] x = d
lookup ((y,v):h) x | x==y = v
                   | otherwise = lookup h x
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
