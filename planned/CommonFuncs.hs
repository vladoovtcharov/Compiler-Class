module CommonFuncs where
import AbstractSyntax
-- here we define some functions which are often used in implementing lambda calculus
-- free variables, substitution, and is value check
--  again we will come back to this later

fv :: Term -> [Var]
fv (BoolConst _)   =  []
fv (IntConst _)    =  []
fv (Var x)         =  [x]
fv (If t1 t2 t3)   =  (fv t1)++(fv t2)++(fv t3)
fv (Abs x _ t1)    =  filter (/=x) (fv t1)
fv (App t1 t2)     =  (fv t1)++(fv t2)


subst :: Var -> Term -> Term -> Term
subst x s (Var v)       = if (v == x) then s else (Var v)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Abs y tau term) = if (x == y || elem y (fv s))
                             then Abs y tau term
                             else Abs y tau (subst x s term)

subst x s (App t1 t2)     = App (subst x s t1) (subst x s t2)
subst x s z  = z

isValue :: Term -> Bool
isValue (Abs _ _ _)    = True
isValue (BoolConst _ ) = True
isValue (IntConst _)   = True
isValue otherwise      = False
