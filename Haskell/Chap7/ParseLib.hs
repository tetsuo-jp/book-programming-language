-- Fig.9.9: Parsing primitives and combinators

module ParseLib where

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v xs = [(v,xs)]

failp :: Parser a b
failp xs = []

satisfy :: (a -> Bool) -> Parser a a
satisfy q [] = failp []
satisfy q (x:xs) | q x = succeed x xs
		 | otherwise = failp xs

literal :: Eq a => a -> Parser a a
literal x = satisfy (==x)

infixr 4 `alt`
alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) xs = p1 xs ++ p2 xs

infixr 6 `seqp`
seqp :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `seqp` p2) xs =
    [((v,v'),xs'')|(v,xs')<-p1 xs, (v',xs'')<-p2 xs']

infixl 5 `using`
using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) xs = [(f v,xs')|(v,xs')<-p xs]

many :: Parser a b -> Parser a [b]
many p = p `seqp` many p `using` uncurry (:) `alt` succeed []

some :: Parser a b -> Parser a [b]
some p = p `seqp` many p `using` uncurry (:)

infixr 6 `x_seq`, `seq_x`
x_seq :: Parser a b -> Parser a c -> Parser a c
p1 `x_seq` p2 = (p1 `seqp` p2) `using` snd
seq_x :: Parser a b -> Parser a c -> Parser a b
p1 `seq_x` p2 = (p1 `seqp` p2) `using` fst

anyof :: (a -> Parser b c) -> [a] -> Parser b c
anyof p = foldr (alt . p) failp
