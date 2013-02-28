\begin{code}
module NaturalSemantics where
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

eval :: S.Term -> S.Term
eval t | (S.isValue t) = t
eval (S.If t1 t2 t3) = case eval t1 of
                         S.BoolConst S.Tru -> eval t2
                         S.BoolConst S.Fls -> eval t3
eval (S.App t1 t2) = case eval t1 of
                       (S.Abs x _ t12) -> eval (S.subst x (eval t2) t12)
                       (S.Fix (S.Abs x _ t12)) -> eval (S.App (S.subst x t1 t12) t2)
eval (S.Let x t1 t2) = eval (S.subst x t1 t2)
eval (S.PrimFunc _ _ f p) =  f (map eval p)
eval otherwise =  error("Evaluation error.")

\end{code}
