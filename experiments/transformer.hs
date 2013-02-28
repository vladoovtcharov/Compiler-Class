
module Transformers where 

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp  = Lit Integer
          | Var Name
          | Plus Exp Exp
          | Abs Name Exp
          | App Exp Exp
          deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving(Show)

type Env = Map.Map Name Value

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

eval0   :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal(i1+i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                             FunVal env' n body -> eval0 (Map.insert n val2 env') body

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1  ev = runIdentity ev

eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal(i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body -> eval1 (Map.insert n val2 env') body
                             
type Eval2 a = ErrorT String Identity a
runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of 
                      Just val -> return val
                      _ -> throwError $ "unbound variable: " ++ n
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                              _ -> throwError "Type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                             FunVal env' n body ->
                               eval2 (Map.insert n val2 env') body
                             _ -> throwError "Type error in application"

type Eval3 a = ReaderT Env (ErrorT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity( runErrorT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Just val -> return val
                     _ -> throwError $ "unbounded variable: " ++ n
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2'<- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
                          _ -> throwError $ "type aerror in additon"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body ->
                           local ( const (Map.insert n val2 env')) (eval3 body)
                         _ -> throwError "type error in aplication"
