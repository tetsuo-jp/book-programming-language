-- 130714作成中

module Sec7_6 where

import Prelude hiding (lookup,lex,seq)
import Data.Char (isSpace)
import ParseLib
import ScanLib
import SemLib
import SynLib
import Domain

-- Section 7.6 Type checking
data T_Expr = T_Int
            | T_Bool
            | T_List T_Expr
            | T_Array (Expr,Expr) T_Expr
            | T_Tuple [T_Expr]
            | T_Struct [(Ide,T_Expr)]
            | T_Union [(Ide,T_Expr)]
            | T_Ref T_Expr
            | T_Fun T_Expr T_Expr

data Typ = Typ_Int
         | Typ_Bool
         | Typ_List Typ
         | Typ_Array Typ Typ
         | Typ_Tuple [Typ]
         | Typ_Struct (Assoc Ide Typ)
         | Typ_Union (Assoc Ide Typ)
         | Typ_Ref Typ
         | Typ_Fun Typ Typ
         | Typ_Wrong
           deriving (Eq,Show)

type Typ_Env = Assoc Ide Typ

exptyp :: Expr -> Typ_Env -> Typ
exptyp (Num n) r = Typ_Int
exptyp (Var x) r = lookup r x   -- p.143
exptyp (Let ds e) r = exptyp e (foldl declenv' r ds) -- p.143
        where
        declenv' r' (Decl x e) = update r' x (exptyp e r)
exptyp (Bexpr o e e') r
        = bintyp (exptyp e r) (exptyp e' r)
        where
        bintyp Typ_Int Typ_Int = Typ_Int
        bintyp _ _ = Typ_Wrong
exptyp (Rexpr o e e') r
        = reltyp (exptyp e r) (exptyp e' r)
        where
        reltyp Typ_Int Typ_Int = Typ_Bool
        reltyp _ _ = Typ_Wrong
exptyp (If e (e',e'')) r        -- p.144
        = iftyp (exptyp e r) (exptyp e' r) (exptyp e'' r)
        where
        iftyp Typ_Bool Typ_Int  Typ_Int  = Typ_Int
        iftyp Typ_Bool Typ_Bool Typ_Bool = Typ_Bool
        iftyp _        _        _        = Typ_Wrong
exptyp (ArrElem e e') r         -- p.155
       = aeltyp (exptyp e r) (exptyp e' r)
       where
       aeltyp (Typ_Array t t') t''
              | t==t'' = t'
              | otherwise = Typ_Wrong
exptyp (StructElem e x) r       -- p.155
       = seltyp (exptyp e r)
       where
       seltyp (Typ_Struct r') = lookup r' x
       seltyp _               = Typ_Wrong
exptyp (Deref e) r = deref (typ e r)
       where
       deref (Typ_Ref t) = t
       deref _           = Typ_Wrong
exptyp (Apply e e') r
       = apply (typ e r) (typ e' r)
       where
       apply (Typ_Fun t t') t'' | t == t'' = t'
                                | otherwise = Typ_Wrong


typ :: T_Expr -> Typ_Env -> Typ
typ T_Int r = Typ_Int
typ T_Bool r = Typ_Bool
typ (T_List t) r = Typ_List (typ t r)
typ (T_Array (e,e') t) r
    | te==Typ_Int && te'==Typ_Int
                = Typ_Array Typ_Int (typ t r)
    | otherwise = Typ_Wrong
    where
    te = exptyp e r
    te' = exptyp e' r
typ (T_Tuple ts) r = Typ_Tuple (map (flip typ r) ts)
typ (T_Struct its) r =
        Typ_Struct (foldl f (assoc Typ_Wrong) its)
        where
        f r' (x,t) = update r' x (typ t r)
typ (T_Union its) r =
        Typ_Union (foldl f (assoc Typ_Wrong) its)
        where
        f r' (x,t) = update r' x (typ t r)
typ (T_Ref t) r = Typ_Ref (typ t r)
typ (T_Fun t t') r = Typ_Fun (typ t r) (typ t' r)

-- Initial environment, p.144
r0 :: Typ_Env
r0 = assoc Typ_Wrong

-- -- Typing function, p.144
-- typ :: [Char] -> Typ
-- typ e = exptyp (parse e) r0

-- Section 4.4
-- Fig.4.3: Expressions with conditionals

-- Note that the order in which strings are enumarated in 'anyof' is
-- significant in case that the same prefices appear in several strings.
lexer :: Parser Char [Token Tag]
lexer = lexime [(some (satisfy isSpace), T_Junk),
                (number, T_Num),
                (string "let", T_Sym),
                (string "in", T_Sym),
                (string "if", T_Sym),
                (string "then", T_Sym),
                (string "else", T_Sym),
                (ident, T_Ide),
                (anyof string ["(",")",
                                "==","/=",">=",">","<=","<",
                                "+","-","*","/","=",";"
                                ], T_Sym) ]

-- Syntactic domains
data Expr = Num Numeral | Var Ide | Bexpr BinOpr Expr Expr | Let [Decl] Expr
          | Rexpr RelOpr Expr Expr | If Expr (Expr,Expr)
          | ArrElem Expr Expr | StructElem Expr Ide | Deref Expr -- p.155
          | Apply Expr Expr     -- p.156
data Decl = Decl Ide Expr

-- Syntax analysis
expr =  lit "if" `x_seq` expr `seqp`
                lit "then" `x_seq` expr `seqp`
                lit "else" `x_seq` expr
                        `using` uncurry If `alt`
        lit "let" `x_seq` decls `seqp`
                lit "in" `x_seq` expr
                        `using` uncurry Let `alt`
        rexpr
rexpr = expr'' `seqp` rexpr' `using` uncurry (flip id)
rexpr'=	lit "==" `x_seq` expr'' `using` flip (Rexpr Equal) `alt`
        lit "/=" `x_seq` expr'' `using` flip (Rexpr NotEqual) `alt`
        lit ">" `x_seq` expr'' `using` flip (Rexpr Greater) `alt`
        lit ">=" `x_seq` expr'' `using` flip (Rexpr GreaterEq) `alt`
        lit "<" `x_seq` expr'' `using` flip (Rexpr Less) `alt`
        lit "<=" `x_seq` expr'' `using` flip (Rexpr LessEq) `alt`
        succeed id
expr''=	term `seqp` expr' `using` uncurry (foldl (flip id))
expr' = many ((lit "+" `x_seq` term `using` flip (Bexpr Plus)) `alt`
                (lit "-" `x_seq` term `using` flip (Bexpr Minus)))
term =	factor `seqp` term' `using` uncurry (foldl (flip id))
term' = many ((lit "*" `x_seq` factor `using` flip (Bexpr Times)) `alt`
                (lit "/" `x_seq` factor `using` flip (Bexpr Over)))
factor =kind T_Num `using` Num `alt`
        kind T_Ide `using` Var `alt`
        lit "(" `x_seq` expr `seq_x` lit ")"
decls = decl `seqp` many (lit ";" `x_seq` decl) `using` uncurry (:)
decl =	kind T_Ide `seqp` lit "=" `x_seq` expr `using` uncurry Decl

-- Parser
type Prog = Expr
prog = expr

lex :: [Char] -> [Token Tag]
lex = (strip T_Junk) . fst . head . lexer

syn :: [Token Tag] -> Prog
syn = fst . head . prog

parse :: [Char] -> Prog
parse = syn . lex

-- Testing, p.144
-- test1 = typ "291+31"
-- test2 = typ "291<31"
-- test3 = typ "let x=2 in let y=x+1; z=x in x+y+z"
-- test4 = typ "let x=0 in if x==0 then x==0 else x-1"

test5 = "begin var x array (0,9) of integer end"