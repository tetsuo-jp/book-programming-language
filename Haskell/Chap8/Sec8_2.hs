module Sec8_2 where
import Data.List                -- nub

-- 8.2 単一化機構

data U_Term a b = U_Var a | U_Struct b [U_Term a b] -- p.171
                deriving Show

-- p.172 (a)代入
null_Subst   :: Subst a b
single_Subst :: Eq a=> a -> U_Term a b -> Subst a b
apply_Subst  :: Eq a => Subst a b -> U_Term a b -> U_Term a b
comp_Subst   :: Eq a => Subst a b -> Subst a b -> Subst a b

type Subst a b = [(a,U_Term a b)]
null_Subst = []
single_Subst x e = [(x,e)]
apply_Subst p e = foldr subst e p
        where
        subst (x,e) e'@(U_Var x')
                   | x==x'     = e
                   | otherwise = e'
        subst q (U_Struct c es) =
                 U_Struct c (map (subst q) es)
comp_Subst p p'= p ++ p'

-- p.173 (b)単一化
type Unifier a b = [Subst a b]

unify_failure :: Unifier a b
unify_failure = []

unify :: Eq a => Eq b => U_Term a b -> U_Term a b -> Unifier a b
unify (U_Var x) e'@(U_Var x')
                   | x==x'     = [null_Subst]
                   | otherwise = [single_Subst x e']
unify (U_Var x) e' | x `elem` u_vars e' = unify_failure
                   | otherwise          = [single_Subst x e']
unify e (U_Var x') | x' `elem` u_vars e = unify_failure
                   | otherwise          = [single_Subst x' e]
unify (U_Struct c es) (U_Struct c' es')
                   | c==c'     = unify_list es es'
                   | otherwise = unify_failure

u_vars :: Eq a => U_Term a b -> [a]
u_vars (U_Var x)       = [x]
u_vars (U_Struct _ es) = (nub . concat . (map u_vars)) es

unify_list :: Eq a => Eq b => [U_Term a b] -> [U_Term a b] -> Unifier a b
unify_list [] []    = [null_Subst]
unify_list [] (_:_) = unify_failure
unify_list (_:_) [] = unify_failure
unify_list (e:es) (e':es') =
    [comp_Subst p' p |
     p <- unify e e',
     p' <- unify_list (map (apply_Subst p) es)
                      (map (apply_Subst p) es')]

test1 = unify (U_Struct "f" [U_Var "X", U_Var "Y"])
            (U_Struct "f" [U_Var "Y", U_Struct "g" [] ])