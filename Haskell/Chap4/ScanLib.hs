-- Fig.9.10: Scanner primitives

module ScanLib where

import ParseLib
import Char (isDigit,isAlpha)

type Token t = (t,[Char])

infix 7 `tok`
tok :: Parser Char [Char] -> t -> Parser Char (Token t)
(p `tok` t) xs = [((t,v),xs')| (v,xs')<-p xs]

lexime :: [(Parser Char [Char], t)] -> Parser Char [Token t]
lexime = many . foldr (alt . uncurry tok) failp

strip :: Eq t => t -> [Token t] -> [Token t]
strip t = filter ((/= t) . fst)

kind :: Eq t => t -> Parser (Token t) [Char]
kind t = satisfy ((== t) . fst) `using` snd

number :: Parser Char [Char]
number = some (satisfy isDigit)

word :: Parser Char [Char]
word = some (satisfy isAlpha)

string :: Eq a => [a] -> Parser a [a]
string [] = succeed []
string (x:xs) = (literal x `seqp` string xs) `using` uncurry (:)
