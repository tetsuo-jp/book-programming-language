-- Lexical elements and lexical analysis

module SynLib where

import ParseLib
import ScanLib
import Data.Char (isAlpha,isAlphaNum)

-- Lexical elements
data Tag = T_Num | T_Ide | T_Sym | T_Junk deriving Show

instance Eq Tag where
        T_Num==T_Num = True
        T_Ide==T_Ide = True
        T_Sym==T_Sym = True
        T_Junk==T_Junk = True
        _ == _ = False

-- Lexical analysis
lit :: [Char] -> Parser (Token Tag) [Char]
lit xs = literal (T_Sym,xs) `using` snd

ident :: Parser Char [Char]
ident = (satisfy isAlpha `seqp` many (satisfy isAlphaNum)) `using` uncurry (:)

-- 'lexer' depends on the language. An example definition looks like
-- lexer :: Parser Char [Token Tag]
-- lexer = lexime [(some (satisfy isSpace), T_Junk),
--		(number, T_Num),
--		(string "begin", T_Sym),
--		(string "end", T_Sym),
--		(ident, T_Ide),
--		(anyof string ["(",")","+","-"], T_Sym)]
