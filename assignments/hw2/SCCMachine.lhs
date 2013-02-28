One way to simplify the CC machine is to enrich the rules concerning values in the control string
position with information about the structure of the context. A similar simplification is possible 
when the value returned belongs into the function position. The simplified CC machine has fewer transitions.
\begin{code}
module SCCMachine where

import qualified AbstractSyntax as S
import qualified EvaluationContext as E
import qualified IntegerArithmetic as I
\end{code}
Given a state, finding the applicable transition rule is easy. If the control
string is an application, shift a fixed portion to the evaluation context and
concentrate on one sub-expression. If it is a value, check the innermost application
of the evaluation context. If the hole is in the last position of the
application, perform a reduction, otherwise swap information between the
control string (term) and the evaluation context.
\begin{code}
sccMachineStep :: (S.Term, E.Context) -> Maybe (S.Term, E.Context)
sccMachineStep (t, c) =
  case t of
    S.App t1 t2      ->  Just (t1, E.fillWithContext c (E.AppT E.Hole t2))
    S.If t1 t2 t3    ->  Just (t1, E.fillWithContext c (E.If E.Hole t2 t3))
    S.Fix t1         ->  Just (t1, E.fillWithContext c (E.Fix E.Hole))
    S.Let x t1 t2    ->  Just (t1, E.fillWithContext c (E.Let x E.Hole t2))
    S.IntAdd t1 t2   ->  Just (t1, E.fillWithContext c (E.AddT E.Hole t2))
    S.IntSub t1 t2   ->  Just (t1, E.fillWithContext c (E.SubT E.Hole t2))
    S.IntMul t1 t2   ->  Just (t1, E.fillWithContext c (E.MulT E.Hole t2))
    S.IntDiv t1 t2   ->  Just (t1, E.fillWithContext c (E.DivT E.Hole t2))
    S.IntNand t1 t2  ->  Just (t1, E.fillWithContext c (E.NandT E.Hole t2))
    S.IntEq t1 t2    ->  Just (t1, E.fillWithContext c (E.EqT E.Hole t2))
    S.IntLt t1 t2    ->  Just (t1, E.fillWithContext c (E.LtT E.Hole t2))

    t | S.isValue t ->
      case c of
        E.Hole    -> Nothing
        otherwise ->
          case E.lookupHole c of
            (c', E.AppT E.Hole t2)   ->  Just (t2, E.fillWithContext c' (E.AppV t E.Hole))
            (c', E.AppV t1 E.Hole)   ->
                case t1 of (S.Abs x _ t2) -> Just (S.subst x t t2, c')

            (c', E.If E.Hole t2 t3)  ->  case t of
                                           S.Tru -> Just (t2, c')
                                           S.Fls -> Just (t3, c')

            (c', E.Fix E.Hole)       ->
                case t of (S.Abs x _ t2) -> Just (S.subst x (S.Fix t) t2, c')

            (c', E.Let x E.Hole t2)  -> Just (S.subst x t t2, c')

            (c', (E.AddT E.Hole t2))     ->  Just (t2, E.fillWithContext c' (E.AddV t E.Hole))
            (c', (E.AddV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> Just (S.IntConst (I.intAdd t1 t'), c')

            (c', hc@(E.SubT E.Hole t2))  ->  Just (t2, E.fillWithContext c' (E.SubV t E.Hole))
            (c', (E.SubV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> Just (S.IntConst (I.intSub t1 t'), c')

            (c', hc@(E.MulT E.Hole t2))  ->  Just (t2, E.fillWithContext c' (E.MulV t E.Hole))
            (c', (E.MulV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> Just (S.IntConst (I.intMul t1 t'), c')

            (c', hc@(E.DivT E.Hole t2))  ->  Just (t2, E.fillWithContext c' (E.DivV t E.Hole))
            (c', (E.DivV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> Just (S.IntConst (I.intDiv t1 t'), c')

            (c', hc@(E.NandT E.Hole t2)) ->  Just (t2, E.fillWithContext c' (E.NandV t E.Hole))
            (c', (E.NandV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> Just (S.IntConst (I.intNand t1 t'), c')

            (c', hc@(E.EqT E.Hole t2))   ->  Just (t2, E.fillWithContext c' (E.EqV t E.Hole))
            (c', (E.EqV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> case I.intEq t1 t' of
                                             True  -> Just (S.Tru, c')
                                             False -> Just (S.Fls, c')

            (c', hc@(E.LtT E.Hole t2))   ->  Just (t2, E.fillWithContext c' (E.LtV t E.Hole))
            (c', (E.LtV (S.IntConst t1) E.Hole))  ->
                case t of S.IntConst t' -> case I.intLt t1 t' of
                                             True  -> Just (S.Tru, c')
                                             False -> Just (S.Fls, c')

            otherwise                    ->  error "Evaluation error."

\end{code}
------------------------------------------------------------------------------------------------------------------------------\\
NOTE: The original SCC machine does not have these reduction rules but
we can add them to make more shortcuts to improve the efficiency:

< S.App (S.Abs x _ t12) t2                  -> Just (S.subst x t2 t12, c)
< S.If (S.Tru) t2 t3                        -> Just (t2, c)
< S.If (S.Fls) t2 t3                        -> Just (t3, c)
< t@(S.Fix (S.Abs x _ t1))                  -> Just (S.subst x t t1, c)
< (S.Let x t1 t2)
<   | S.isValue t1                          -> Just (S.subst x t1 t2, c)
< S.IntAdd (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intAdd t1 t2), c)
< S.IntSub (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intSub t1 t2), c)
< S.IntMul (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intMul t1 t2), c)
< S.IntDiv (S.IntConst t1) (S.IntConst t2)  -> Just (S.IntConst (I.intDiv t1 t2), c)
< S.IntNand (S.IntConst t1) (S.IntConst t2) -> Just (S.IntConst (I.intNand t1 t2), c)
< S.IntEq (S.IntConst t1) (S.IntConst t2)   -> case I.intEq t1 t2 of
<                                                True  -> Just (S.Tru, c)
<                                                False -> Just (S.Fls, c)
< S.IntLt (S.IntConst t1) (S.IntConst t2)   -> case I.intLt t1 t2 of
<                                                True  -> Just (S.Tru, c)
<                                                False -> Just (S.Fls, c)
------------------------------------------------------------------------------------------------------------------------------
\begin{code}

sccMachineEval' :: (S.Term, E.Context) -> (S.Term, E.Context)
sccMachineEval' (t, c) = case sccMachineStep (t, c) of
                          Just (t', c') -> sccMachineEval' (t', c')
                          Nothing       -> (t, E.Hole)

sccMachineEval :: S.Term -> S.Term
sccMachineEval t = fst (sccMachineEval' (t, E.Hole))
\end{code}
