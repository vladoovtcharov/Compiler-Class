-- an implementation of hindley milner type inference

module Typing where
import Data.Maybe
import Data.List
import qualified AbstractSyntax as S

data Context = Empty
             | Bind Context S.Var S.Type
             deriving Eq

instance Show Context where
  show Empty        = "<>"
  show (Bind capGamma x t) = show capGamma ++ "," ++ x ++ ":" ++ show t

contextLookup :: S.Var -> Context -> Maybe S.Type
contextLookup x Empty = Nothing
contextLookup x (Bind capGamma y t)
  | x == y    = Just t
  | otherwise = contextLookup x capGamma

typing :: Context -> S.Term -> Maybe S.Type
typing capGamma t = case t of
      S.Var x        -> contextLookup x capGamma

      S.Abs x t1 t2  -> do t3 <- typing (Bind capGamma x t1) t2
                           Just (S.TypeArrow t1 t3)

      S.App t1 t2    -> do (S.TypeArrow t3 t4) <- typing capGamma t1
                           t <- typing capGamma t2
                           if t == t3 then Just t4 else Nothing

      S.BoolConst _          -> Just S.TypeBool

      S.If t1 t2 t3  -> do S.TypeBool <- typing capGamma t1
                           t <- typing capGamma t2
                           tau <- typing capGamma t3
                           if tau == t then Just t else Nothing

      S.IntConst _    -> Just S.TypeInt
      S.IntAdd t1 t2  -> arith t1 t2
      S.IntSub t1 t2  -> arith t1 t2
      S.IntMul t1 t2  -> arith t1 t2
      S.IntDiv t1 t2  -> arith t1 t2
      S.IntNand t1 t2 -> arith t1 t2
      S.IntEq t1 t2   -> rel t1 t2
      S.IntLt t1 t2   -> rel t1 t2
      where
        arith t1 t2 = do S.TypeInt <- typing capGamma t1
                         S.TypeInt <- typing capGamma t2
                         Just S.TypeInt
        rel t1 t2 = do S.TypeInt <- typing capGamma t1
                       S.TypeInt <- typing capGamma t2
                       Just S.TypeBool

typeCheck :: S.Term -> S.Type
typeCheck t =
  case typing Empty t of
    Just t -> t
    _ -> error "type error"

