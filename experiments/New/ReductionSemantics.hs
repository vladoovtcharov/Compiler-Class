module ReductionSemantics where
import qualified AbstractSyntax as S
import qualified EvaluationContext as E
import qualified IntegerArithmetic as I
import Debug.Trace
makeEvalContext :: S.Term -> Maybe (S.Term, E.Context)
makeEvalContext t = case t of
  S.App (S.Abs x tau11 t12) t2
    |  S.isValue t2 -> Just (t, E.Hole)
  S.App t1 t2
    |  S.isValue t1 -> Just (t2, E.AppV t1 E.Hole)
    |  otherwise -> Just (t1, E.AppT E.Hole t2)
  S.If (S.Tru) t2 t3 -> Just (t, E.Hole)
  S.If (S.Fls) t2 t3 -> Just (t, E.Hole)
  S.If t1 t2 t3 -> Just (t1, E.If E.Hole t2 t3)
  S.IntAdd t1@(S.IntConst _) t2@(S.IntConst _) -> Just(t, E.Hole)
  S.IntAdd t1@(S.IntConst _) t2 -> Just (t2, E.BinaryOpV t1 E.Hole S.IntAdd)
  S.IntAdd t1 t2              -> Just (t1, E.BinaryOpT E.Hole t2 S.IntAdd)
  _ -> Nothing

makeContractum :: S.Term -> Maybe S.Term
makeContractum t = case t of
  S.App (S.Abs x tau11 t12) t2               ->  Just $ S.subst x t2 t12
  S.IntAdd (S.IntConst t1) (S.IntConst t2)   ->  Just $ S.IntConst (I.intAdd t1 t2)
  S.If S.Tru t1 t2 -> Just t1
  S.If S.Fls t1 t2 -> Just t2
  _ -> Nothing

textualMachineStep :: S.Term -> Maybe S.Term
textualMachineStep t = do
  -- trace (show t) (Just t)
  -- make a context to evaluate in
  (t', c) <- makeEvalContext t

  -- if it is a redex return the reduced version
  -- else search further in the term
  case makeContractum t' of
    Just t  -> return t
    Nothing -> do (nt, c') <- makeEvalContext t'
                  textualMachineStep (E.fillWithTerm (E.fillWithContext c c') nt)

textualMachineEval :: S.Term -> S.Term
textualMachineEval t =
  case textualMachineStep t of
    Just t' -> textualMachineEval t'
    Nothing -> t

