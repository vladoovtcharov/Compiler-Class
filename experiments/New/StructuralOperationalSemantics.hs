module StructuralOperationalSemantics where
import Control.Monad.Error
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I


eval1 :: S.Term -> S.Term
eval1 x | S.isValue x = x
eval1 (S.If (S.BoolConst S.Tru) t2 t3) = t2
eval1 (S.If (S.BoolConst S.Fls) t2 t3) = t3
eval1 (S.If t1 t2 t3) = S.If (eval1 t1) t2 t3
eval1 (S.PrimFunc n t f p) = 
  case span (S.isValue) p of
    (fst, next:snd) -> S.PrimFunc n t f (fst ++ [(eval1 next)] ++ snd)
    (fst, []) -> f p
eval1 (S.App t1 t2) = case t1 of
  (S.Abs x tau1 t) -> S.subst x t2 t
  (S.Fix (S.Abs x tau1 t)) -> S.App (S.subst x t1 t) t2
  (S.Fix t) -> S.App (S.Fix (eval1 t)) t2
  t -> S.App (eval1 t) t2
eval1 (S.Let x t1 t2) = S.subst x t1 t2

eval :: S.Term -> IO S.Term
eval t = case eval1 t of
           t' | S.isValue t' -> return t'
           otherwise -> eval t
