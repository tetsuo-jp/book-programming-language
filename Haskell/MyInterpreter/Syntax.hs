module Syntax where

data Expr = Num Numeral
	  | Var Ide
	  | Bexpr BinOpr Expr Expr
	  | Rexpr RelOpr Expr Expr
	  | Let [Decl] Expr
	  | If Expr (Expr,Expr)
	  | Fun Ide Expr
	  | Apply Expr Expr
	  | E_Bool Boolean
		deriving Show
data Decl = Decl Ide Expr deriving Show
type Numeral = String
type Ide = String
type Boolean = String
data BinOpr = Plus | Minus | Times | Over
		deriving Show
data RelOpr = Equal | NotEqual | Greater | GreaterEq | Less | LessEq
		deriving Show
data Val = V_Int Int
	 | V_Bool Bool
	 | V_Fun (Val -> Val)
type Env = Assoc Ide Val

instance Show Val where
	showsPrec p (V_Int n) = showsPrec p n
	showsPrec p (V_Bool b) = showsPrec p b
	showsPrec p (V_Fun f) = showString "<function>"

-- Abstract type 'Assoc' defined in Chapter 2
none :: Assoc a b
assoc :: b -> Assoc a b
aLookup :: Eq a => Assoc a b -> a -> b
update :: Eq a => Assoc a b -> a -> b -> Assoc a b

-- Implementation in terms of lists of pairs
type Assoc a b = [(a,b)]
none = []
assoc d = [(undefined,d)]
aLookup [] x = undefined
aLookup [(_,d)] x = d
aLookup ((y,v):h) x
	| x==y = v
	| otherwise = aLookup h x
update h x v = (x,v) : h
