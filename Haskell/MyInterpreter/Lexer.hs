module Lexer(Token(..),lexer) where

import Syntax
import Data.Char

data Token
        = T_If | T_Then | T_Else
        | T_Let | T_In
        | T_Int String
        | T_Var String
        | T_Eq
        | T_Plus | T_Minus | T_Times | T_Div
        | T_OB | T_CB
        | T_Semi
        | T_GT | T_LT | T_LE | T_GE | T_EQ | T_NE
        | T_Bool String
        | T_And | T_Or
          deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexNum (c:cs)
lexer ('&':'&':cs) = T_And : lexer cs
lexer ('|':'|':cs) = T_Or : lexer cs

lexer ('>':'=':cs) = T_GE : lexer cs
lexer ('<':'=':cs) = T_LE : lexer cs
lexer ('>':cs) = T_GT : lexer cs
lexer ('<':cs) = T_LT : lexer cs
lexer ('=':'=':cs) = T_EQ : lexer cs
lexer ('/':'=':cs) = T_NE : lexer cs

lexer ('+':cs) = T_Plus : lexer cs
lexer ('-':cs) = T_Minus : lexer cs
lexer ('*':cs) = T_Times : lexer cs
lexer ('/':cs) = T_Div : lexer cs

lexer ('=':cs) = T_Eq : lexer cs
lexer ('(':cs) = T_OB : lexer cs
lexer (')':cs) = T_CB : lexer cs
lexer (';':cs) = T_Semi : lexer cs

lexNum cs = T_Int num : lexer rest
        where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
        ("if",rest)   -> T_If : lexer rest
        ("then",rest) -> T_Then : lexer rest
        ("else",rest) -> T_Else : lexer rest
        ("let",rest)  -> T_Let : lexer rest
        ("in",rest)   -> T_In : lexer rest
        ("True",rest) -> T_Bool "True" : lexer rest
        ("False",rest) -> T_Bool "False" : lexer rest
        (var,rest)    -> T_Var var : lexer rest
