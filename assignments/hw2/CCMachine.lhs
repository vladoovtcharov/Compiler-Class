The evaluation process on a CC machine consists of shifting pieces of the control 
string into the evaluation context such that the control string becomes a redex. 
Once the control string has turned into a redex, an ordinary contraction occurs. 
If the result is a value, the machine must shift pieces of the evaluation context 
back to the control string.
\begin{code}
module CCMachine where

import qualified AbstractSyntax as S
import qualified EvaluationContext as E
import qualified IntegerArithmetic as I

ccMachineStep :: (S.Term, E.Context) -> Maybe (S.Term, E.Context)
ccMachineStep (t, c) =
  case t of
    S.App t1 t2
      | not (S.isValue t1)                    -> Just (t1, E.fillWithContext c (E.AppT E.Hole t2))
      | S.isValue t1 && not (S.isValue t2)    -> Just (t2, E.fillWithContext c (E.AppV t1 E.Hole))
    S.App (S.Abs x _ t12) t2                  -> Just (S.subst x t2 t12, c)

    S.If (S.Tru) t2 t3                        -> Just (t2, c)
    S.If (S.Fls) t2 t3                        -> Just (t3, c)
    S.If t1 t2 t3                             -> Just (t1, E.fillWithContext c (E.If E.Hole t2 t3))

    t@(S.Fix (S.Abs x _ t1))                  -> Just (S.subst x t t1, c)
    S.Fix t                                   -> Just (t, E.fillWithContext c (E.Fix E.Hole))

    (S.Let x t1 t2)
      | S.isValue t1                          -> Just (S.subst x t1 t2, c)
      | otherwise                             -> Just (t1, E.fillWithContext c (E.Let x E.Hole t2))

    S.IntAdd (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intAdd t1 t2), c)
    S.IntAdd t1@(S.IntConst _) t2             -> Just (t2, E.fillWithContext c (E.AddV t1 E.Hole))
    S.IntAdd t1 t2                            -> Just (t1, E.fillWithContext c (E.AddT E.Hole t2))

    S.IntSub (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intSub t1 t2), c)
    S.IntSub t1@(S.IntConst _) t2             -> Just (t2, E.fillWithContext c (E.SubV t1 E.Hole))
    S.IntSub t1 t2                            -> Just (t1, E.fillWithContext c (E.SubT E.Hole t2))

    S.IntMul (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intMul t1 t2), c)
    S.IntMul t1@(S.IntConst _) t2             -> Just (t2, E.fillWithContext c (E.MulV t1 E.Hole))
    S.IntMul t1 t2                            -> Just (t1, E.fillWithContext c (E.MulT E.Hole t2))

    S.IntDiv (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intDiv t1 t2), c)
    S.IntDiv t1@(S.IntConst _) t2             -> Just (t2, E.fillWithContext c (E.DivV t1 E.Hole))
    S.IntDiv t1 t2                            -> Just (t1, E.fillWithContext c (E.DivT E.Hole t2))

    S.IntNand (S.IntConst t1) (S.IntConst t2) -> Just (S.IntConst (I.intNand t1 t2), c)
    S.IntNand t1@(S.IntConst _) t2            -> Just (t2, E.fillWithContext c (E.NandV t1 E.Hole))
    S.IntNand t1 t2                           -> Just (t1, E.fillWithContext c (E.NandT E.Hole t2))

    S.IntEq (S.IntConst t1) (S.IntConst t2)   -> case I.intEq t1 t2 of
                                                   True  -> Just (S.Tru, c)
                                                   False -> Just (S.Fls, c)
    S.IntEq t1@(S.IntConst _) t2              -> Just (t2, E.fillWithContext c (E.EqV t1 E.Hole))
    S.IntEq t1 t2                             -> Just (t1, E.fillWithContext c (E.EqT E.Hole t2))

    S.IntLt (S.IntConst t1) (S.IntConst t2)   -> case I.intLt t1 t2 of
                                                   True  -> Just (S.Tru, c)
                                                   False -> Just (S.Fls, c)
    S.IntLt t1@(S.IntConst _) t2              -> Just (t2, E.fillWithContext c (E.LtV t1 E.Hole))
    S.IntLt t1 t2                             -> Just (t1, E.fillWithContext c (E.LtT E.Hole t2))

    t | S.isValue t ->
      case c of
        E.Hole    -> Nothing
        otherwise -> case E.lookupHole c of
                       (c', hc@(E.AppT E.Hole t2))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.AppV t1 E.Hole))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.If E.Hole t2 t3)) ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.Fix E.Hole))      ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.Let x E.Hole t2)) ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.AddT E.Hole t2))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.AddV t1 E.Hole))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.SubT E.Hole t2))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.SubV t1 E.Hole))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.MulT E.Hole t2))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.MulV t1 E.Hole))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.DivT E.Hole t2))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.DivV t1 E.Hole))  ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.NandT E.Hole t2)) ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.NandV t1 E.Hole)) ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.EqT E.Hole t2))   ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.EqV t1 E.Hole))   ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.LtT E.Hole t2))   ->  Just (E.fillWithTerm hc t, c')
                       (c', hc@(E.LtV t1 E.Hole))   ->  Just (E.fillWithTerm hc t, c')
                       otherwise                    ->  error "Evaluation error."

ccMachineEval' :: (S.Term, E.Context) -> (S.Term, E.Context)
ccMachineEval' (t, c) = case ccMachineStep (t, c) of
                          Just (t', c') -> ccMachineEval' (t', c')
                          Nothing       -> (t, E.Hole)

ccMachineEval :: S.Term -> S.Term
ccMachineEval t = fst (ccMachineEval' (t, E.Hole))
\end{code}
