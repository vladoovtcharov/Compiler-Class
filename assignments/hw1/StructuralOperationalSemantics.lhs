\begin{code}
module StructuralOperationalSemantics where
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

eval1 :: S.Term -> Maybe S.Term
eval1 t = case t of
            (S.If S.Tru t2 t3) -> Just t2
            (S.If S.Fls t2 t3) -> Just t3
            (S.If t1 t2 t3)    ->
                do t1' <- eval1 t1
                   Just (S.If t1' t2 t3)

            (S.App (S.Abs x _ t12) t2)
                | S.isValue t2 -> Just (S.subst x t2 t12)

            (S.App t1 t2)
                | S.isValue t1 ->
                    do t2' <- eval1 t2
                       Just (S.App t1 t2')
                | S.isValue t2 ->
                    do t1' <- eval1 t1
                       Just (S.App t1' t2)

            (S.IntAdd t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   Just (S.IntConst (I.intAdd t1' t2'))

            (S.IntSub t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   Just (S.IntConst (I.intSub t1' t2'))

            (S.IntMul t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   Just (S.IntConst (I.intMul t1' t2'))

            (S.IntDiv t1 t2) ->
                do
                   (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   Just (S.IntConst (I.intDiv t1' t2'))

            (S.IntNand t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   Just (S.IntConst (I.intNand t1' t2'))

            (S.IntEq t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   case (I.intEq t1' t2') of
                     True  -> Just S.Tru
                     False -> Just S.Fls

            (S.IntLt t1 t2) ->
                do (S.IntConst t1') <- getInt t1
                   (S.IntConst t2') <- getInt t2
                   case (I.intLt t1' t2') of
                     True  -> Just S.Tru
                     False -> Just S.Fls

            otherwise -> Nothing

eval :: S.Term -> S.Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

getInt :: S.Term -> Maybe S.Term
getInt t = if S.isValue t then
              case t of
                (S.IntConst _) -> Just t
                otherwise      -> Nothing
           else eval1 t
\end{code}
