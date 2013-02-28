module StructuralOperationalSemantics where
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I


binArith :: (S.Term -> S.Term -> S.Term) 
            -> S.Term -> S.Term
            -> (S.IntConst -> S.IntConst -> S.IntConst) 
            -> Maybe S.Term
binArith f (S.IntConst t1) (S.IntConst t2) op = Just $ S.IntConst $ op t1 t2
binArith f t1@(S.IntConst _) t2 op            = do t2' <- eval1 t2
                                                   return $ f t1 t2'
binArith f t1 t2 _                            = do t1' <- eval1 t1
                                                   return $ f t1' t2

binArithRel :: (S.Term -> S.Term -> S.Term) 
               -> S.Term -> S.Term 
               -> (S.IntConst -> S.IntConst -> Bool) 
               -> Maybe S.Term
binArithRel f (S.IntConst t1) (S.IntConst t2) op = case op t1 t2 of
                                                     True  -> Just S.Tru
                                                     False -> Just S.Fls 
binArithRel f t1@(S.IntConst _) t2 op            = do t2' <- eval1 t2
                                                      return $ f t1 t2'
binArithRel f t1 t2 _                            = do t1' <- eval1 t1
                                                      return $ f t1' t2


eval1 :: S.Term -> Maybe S.Term
eval1 x | S.isValue x = Just x
eval1 (S.If S.Tru t2 t3) = Just t2
eval1 (S.If S.Fls t2 t3) = Just t3
eval1 (S.If t1 t2 t3) = do t1' <- eval1 t1; return $ S.If t1' t2 t3
eval1 (S.IntAdd t1 t2) = binArith (S.IntAdd) t1 t2 (I.intAdd)
eval1 (S.IntSub t1 t2) = binArith (S.IntSub) t1 t2 (I.intSub)
eval1 (S.IntMul t1 t2) = binArith (S.IntMul) t1 t2 (I.intMul)
eval1 (S.IntDiv t1 t2) = binArith (S.IntDiv) t1 t2 (I.intDiv)
eval1 (S.IntNand t1 t2) = binArith (S.IntNand) t1 t2 (I.intNand)
eval1 (S.IntEq t1 t2) = binArithRel (S.IntEq) t1 t2 (I.intEq)
eval1 (S.IntLt t1 t2) = binArithRel (S.IntLt) t1 t2 (I.intLt)
eval1 (S.App t1 t2) = 
  case eval1 t1 of
  Just (S.Abs x tau11 t12) ->
    do t2' <- eval1 t2
       return $ S.subst x t2' t12
  Just (S.Fix (S.Abs x _ t)) ->
    Just $ S.App (S.subst x t1 t) t2
eval1 (S.Let x t1 t2) = Just $ S.subst x t1 t2
eval1 t = Nothing




eval :: S.Term -> IO S.Term
eval t = do
  -- print $ show t
  case eval1 t of 
    Just t' | S.isValue t' -> do return t'
    Just t' -> eval t'
    _ -> error $ "couldn't eval: " ++ show t


--type Eval a = (WriterT [String] IO) a
--runEval :: Eval a -> IO (a, [String])
--runEval ev = runWriterT ev

--eval :: S.Term -> IO S.Term
--eval t = do  (t', h) <- runEval(eval' t)
--             return t'

