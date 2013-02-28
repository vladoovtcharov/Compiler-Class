module CCMachine where

import qualified AbstractSyntax as S
import qualified EvaluationContext as E
import qualified IntegerArithmetic as I
ccMachineStep :: (S.Term, E.Context) -> Maybe (S.Term, E.Context)
ccMachineStep (t,c) = case t of
  S.App t1 t2
    | (not . S.isValue) t1  -> Just (t1 , E.fillWithContext c (E.AppT E.Hole t2))
    | S.isValue t1 && (not . S.isValue) t2 -> Just (t2, E.fillWithContext c (E.AppV t1 E.Hole))
  S.App (S.Abs x _ t12) t2 -> Just (S.subst x t2 t12, c)
  S.If t1 t2 t3
    | (not . S.isValue) t1 -> Just (t1 , E.fillWithContext c (E.If E.Hole t2 t3))
  S.If (S.Tru) t2 t3 -> Just (t2, c)
  S.If (S.Fls) t2 t3 -> Just (t3, c)
  S.IntAdd t1 t2 
    | (not . S.isValue) t1 -> Just (t1, E.fillWithContext c (E.BinaryOpT S.IntAdd E.Hole t2))
    | S.isValue t1 && (not . S.isValue) t2 -> Just (t2, E.fillWithContext c (E.BinaryOpV S.IntAdd t1 E.Hole))
  S.IntAdd (S.IntConst t1) (S.IntConst t2) -> Just (S.IntConst (I.intAdd t1 t2) , c)
  v | S.isValue v -> Just( E.fillWithTerm c v , E.Hole)

ccMachineEval :: S.Term -> S.Term
ccMachineEval t = 
  case ccMachineStep (t, E.Hole) of
    Just (t, E.Hole) | S.isValue t -> t
    Just (t, c) -> ccMachine(t,c)
    _ -> error $ "couldn't eval" ++ show t
