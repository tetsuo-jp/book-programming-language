module Domain where

import SemLib

-- Semantic domains for imperative languages

data Val = V_Int Int | V_Bool Bool | V_Loc Loc
	 | V_Cont Cont | V_Proc Proc | V_Func Func
	 | V_Thunk (E_Cont -> Cont)
type Loc = Int
type Store = Assoc Loc Val
type State = (Loc, Store)
type Env = Assoc Ide Val
type Ans = [Val]
type Cont = State -> Ans
type E_Cont = Val -> Cont
type Proc = [Val] -> Cont -> Cont
type Func = [Val] -> E_Cont -> Cont
type Thunk = E_Cont -> Cont

instance Show Val where
    showsPrec p (V_Int n)  = showsPrec p n
    showsPrec p (V_Bool b) = showsPrec p b
    showsPrec p (V_Loc l)  = showString "Loc:" . showsPrec p l
    showsPrec p (V_Cont _) = showString "<cont>"
    showsPrec p (V_Proc _) = showString "<proc>"
    showsPrec p (V_Func _) = showString "<func>"
    showsPrec p (V_Thunk _) = showString "<thunk>"