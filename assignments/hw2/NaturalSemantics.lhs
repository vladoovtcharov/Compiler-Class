\begin{code}
module NaturalSemantics where
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

eval :: S.Term -> S.Term
eval t = if (S.isValue t) then t
         else
            case t of
              (S.If t1 t2 t3)   -> case eval t1 of
                                     S.Tru -> eval t2
                                     S.Fls -> eval t3

              (S.App t1 t2)     -> case eval t1 of
                                     (S.Abs x _ t12) -> eval (S.subst x (eval t2) t12)

              a@(S.Fix t)       -> case t of
                                    (S.Abs x _ t1) -> eval (S.subst x a t1)
                                    otherwise -> eval (S.Fix (eval t))

              (S.Let x t1 t2)   -> eval (S.subst x t1 t2)
              (S.IntAdd t1 t2)  -> (S.IntConst (I.intAdd (getInt t1) (getInt t2)))
              (S.IntSub t1 t2)  -> (S.IntConst (I.intSub (getInt t1) (getInt t2)))
              (S.IntMul t1 t2)  -> (S.IntConst (I.intMul (getInt t1) (getInt t2)))
              (S.IntDiv t1 t2)  -> (S.IntConst (I.intDiv (getInt t1) (getInt t2)))
              (S.IntNand t1 t2) -> (S.IntConst (I.intNand (getInt t1) (getInt t2)))

              (S.IntEq t1 t2)   -> case (I.intEq (getInt t1) (getInt t2)) of
                                     True  -> S.Tru
                                     False -> S.Fls

              (S.IntLt t1 t2)   -> case (I.intLt (getInt t1) (getInt t2)) of
                                     True  -> S.Tru
                                     False -> S.Fls

              otherwise         -> error("Evaluation error.")

getInt :: S.Term -> Integer
getInt t = case eval t of
             (S.IntConst t') -> t'
             otherwise       -> error("Evaluation error.")
\end{code}
