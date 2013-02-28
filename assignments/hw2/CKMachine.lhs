Each step of an evaluation on the CC or SCC
machine refers to the innermost application of the evaluation context,
that is, the application that directly contains the hole. Consequently,
the transitions always access the evaluation context from the inside and in
a last-in, first-out fashion. The difference between the SCC and CK machine is one of data structure representation.
Hence, all we need is a way to find a corresponding CK state for
each SCC state, and vice versa.
\begin{code}
module CKMachine where

import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

data Cont  =  MachineTerminate
           |  Fun     S.Term Cont  -- where Term is a value
           |  Arg     S.Term Cont
           |  If      S.Term S.Term Cont
           |  Fix     Cont
           |  Let     S.Var S.Term Cont
           |  Plus1   S.Term Cont
           |  Plus2   S.Term Cont  -- where Term is a value
           |  Minus1  S.Term Cont
           |  Minus2  S.Term Cont  -- where Term is a value
           |  Times1  S.Term Cont
           |  Times2  S.Term Cont  -- where Term is a value
           |  Div1    S.Term Cont
           |  Div2    S.Term Cont  -- where Term is a value
           |  Nand1   S.Term Cont
           |  Nand2   S.Term Cont  -- where Term is a value
           |  Eq1     S.Term Cont
           |  Eq2     S.Term Cont  -- where Term is a value
           |  Lt1     S.Term Cont
           |  Lt2     S.Term Cont  -- where Term is a value


ckMachineStep :: (S.Term, Cont) -> Maybe (S.Term, Cont)
ckMachineStep (t, k) =
    case t of
      S.App t1 t2      ->  Just (t1, Arg t2 k)
      S.If t1 t2 t3    ->  Just (t1, If t2 t3 k)
      S.Fix t1         ->  Just (t1, Fix k)
      S.Let x t1 t2    ->  Just (t1, Let x t2 k)
      S.IntAdd t1 t2   ->  Just (t1, Plus1 t2 k)
      S.IntSub t1 t2   ->  Just (t1, Minus1 t2 k)
      S.IntMul t1 t2   ->  Just (t1, Times1 t2 k)
      S.IntDiv t1 t2   ->  Just (t1, Div1 t2 k)
      S.IntNand t1 t2  ->  Just (t1, Nand1 t2 k)
      S.IntEq t1 t2    ->  Just (t1, Eq1 t2 k)
      S.IntLt t1 t2    ->  Just (t1, Lt1 t2 k)

      t | S.isValue t ->
          case k of
            MachineTerminate         ->  Nothing
            Arg t2 k                 ->  Just (t2, Fun t k)
            Fun (S.Abs x _ t1) k     ->  Just (S.subst x t t1, k)
            If t2 t3 k               ->  case t of
                                           S.Tru -> Just (t2, k)
                                           S.Fls -> Just (t3, k)
            Fix k                    ->  case t of
                                           (S.Abs x _ t2) -> Just (S.subst x (S.Fix t) t2, k)
            Let x t2 k               ->  Just (S.subst x t t2, k)
            Plus1 t2 k               ->  Just (t2, Plus2 t k)
            Plus2 (S.IntConst t1) k  ->
                case t of (S.IntConst t2) -> Just (S.IntConst (I.intAdd t1 t2), k)

            Minus1 t2 k              ->  Just (t2, Minus2 t k)
            Minus2 (S.IntConst t1) k ->
                case t of (S.IntConst t2) -> Just (S.IntConst (I.intSub t1 t2), k)

            Times1 t2 k              ->  Just (t2, Times2 t k)
            Times2 (S.IntConst t1) k ->
                case t of (S.IntConst t2) -> Just (S.IntConst (I.intMul t1 t2), k)

            Div1 t2 k                ->  Just (t2, Div2 t k)
            Div2 (S.IntConst t1) k   ->
                case t of (S.IntConst t2) -> Just (S.IntConst (I.intDiv t1 t2), k)

            Nand1 t2 k               ->  Just (t2, Nand2 t k)
            Nand2 (S.IntConst t1) k  ->
                case t of (S.IntConst t2) -> Just (S.IntConst (I.intNand t1 t2), k)

            Eq1 t2 k                 ->  Just (t2, Eq2 t k)
            Eq2 (S.IntConst t1) k    ->
                case t of (S.IntConst t2) -> case I.intEq t1 t2 of
                                               True  -> Just (S.Tru, k)
                                               False -> Just (S.Fls, k)

            Lt1 t2 k                 ->  Just (t2, Lt2 t k)
            Lt2 (S.IntConst t1) k    ->
                case t of (S.IntConst t2) -> case I.intLt t1 t2 of
                                               True  -> Just (S.Tru, k)
                                               False -> Just (S.Fls, k)

ckMachineEval' :: (S.Term, Cont) -> (S.Term, Cont)
ckMachineEval' (t, k) = case ckMachineStep (t, k) of
                          Just (t', k') -> ckMachineEval' (t', k')
                          Nothing       -> (t, MachineTerminate)

ckMachineEval :: S.Term -> S.Term
ckMachineEval t = fst (ckMachineEval' (t, MachineTerminate))
\end{code}
