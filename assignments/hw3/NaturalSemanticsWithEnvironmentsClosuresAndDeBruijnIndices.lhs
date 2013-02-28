For evaluation of Fix  t, we first wrap \textit{Fix  t} in a closure and then we put it in the environment:
\begin{spec}
a@(B.Fix t1)  ->  case evalInEnv e t1 of
                    Just (Clo (B.Abs _ t1') e1')  ->  evalInEnv ((Clo  a  e1'):e)  t1'
                    otherwise                     ->  Nothing
\end{spec}

\begin{code}
module NaturalSemanticsWithEnvironmentsClosuresAndDeBruijnIndices where
import Data.Maybe
import qualified DeBruijn as B
import qualified IntegerArithmetic as I

data Value = BoolVal Bool | IntVal Integer | Clo B.Term Env

instance Show Value where
  show (BoolVal b) = show b
  show (IntVal i)  = show i
  show (Clo t e)   = "Function: Clo " ++ show t ++ " " ++ show e

type Env = [Value]

evalInEnv :: Env -> B.Term -> Maybe Value
evalInEnv e t =
    case t of
      B.Tru             -> Just (BoolVal True)
      B.Fls             -> Just (BoolVal False)
      (B.IntConst i)    -> Just (IntVal i)
      (B.Var i)         -> if (length e > i)
                           then case e!!i of
                                  (Clo t' e') -> evalInEnv e' t'
                                  v           -> Just v
                           else error("Invalid environment!")

      a@(B.Abs _ _)     -> Just (Clo a e)
      (B.If t1 t2 t3)   -> case evalInEnv e t1 of
                             Just (BoolVal True)  -> evalInEnv e t2
                             Just (BoolVal False) -> evalInEnv e t3

      (B.App t1 t2)     -> case evalInEnv e t1 of
                             Just (Clo (B.Abs _ t12) e1) ->
                                case evalInEnv e t2 of
                                  Just v -> evalInEnv (v:e1) t12
                                  otherwise -> Nothing
                             otherwise -> Nothing

      a@(B.Fix t1)      -> case evalInEnv e t1 of
                             Just (Clo (B.Abs _ t1') e1') ->
                                evalInEnv ((Clo a e1'):e) t1'
                             otherwise -> Nothing

      (B.Let t1 t2)     -> case evalInEnv e t1 of
                             Just v    -> evalInEnv (v:e) t2
                             otherwise -> Nothing

      (B.IntAdd t1 t2)  -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (IntVal (I.intAdd i1 i2))

      (B.IntSub t1 t2)  -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (IntVal (I.intSub i1 i2))

      (B.IntMul t1 t2)  -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (IntVal (I.intMul i1 i2))

      (B.IntDiv t1 t2)  -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (IntVal (I.intDiv i1 i2))

      (B.IntNand t1 t2) -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (IntVal (I.intNand i1 i2))

      (B.IntLt t1 t2)   -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (BoolVal (I.intLt i1 i2))

      (B.IntEq t1 t2)   -> do (IntVal i1) <- evalInEnv e t1
                              (IntVal i2) <- evalInEnv e t2
                              Just (BoolVal (I.intEq i1 i2))

eval :: B.Term -> Value
eval t = case (evalInEnv [] t) of
           Just v    -> v
           otherwise -> error("Evaluation error.")
\end{code}
