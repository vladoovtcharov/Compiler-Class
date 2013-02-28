module StructuralOperationalSemantics where
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

eval1 :: S.Term -> Maybe S.Term
eval1 t = case t of
  (S.If (S.BoolConst True) t2 t3) -> Just t2
  (S.If (S.BoolCOnst False) t2 t3) -> Just t3
  (S.If t1 t2 t3) -> eval1 t1 >>= (\x -> Just (S.If x t2 t3))

  (S.App (S.Abs x _ t12) t2)
     | S.isValue t2 -> Just (S.subst x t2 t12)
  (S.App t1 t2)
     | (not . S.isValue) t1 -> eval1 t1 >>= (\x -> Just (S.App x t2))
     | S.isValue t1 -> eval1 t2 >>= (\x -> Just (S.App t1 x))

  (S.IntAdd (S.IntConst t1) (S.IntConst t2)) -> Just (S.IntConst (I.intAdd t1 t2))
  (S.IntAdd t1 t2)
     | (not . S.isValue) t1 -> eval1 t1 >>= (\x -> Just (S.IntAdd x t2))
     | S.isValue t1 -> eval1 t2 >>= (\x -> Just (S.IntAdd t1 x))

  otherwise -> Nothing

eval :: S.Term -> S.Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
