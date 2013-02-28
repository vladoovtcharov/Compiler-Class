module TypeCheck where
import Data.Maybe
import Data.List
import qualified AbstractSyntax as S
import Control.Monad
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
  S.Abs x tau1 t2  -> do tau2 <- typing (Bind capGamma x tau1) t2
                         Just (S.TypeArrow tau1 tau2)
  S.Let x t1 t2  -> do tau1 <- typing capGamma t1
                       typing (Bind capGamma x tau1) t2
  S.Fix t1-> do (S.TypeArrow tau1 tau2) <-  typing capGamma t1
                Just tau2
  S.App t1 t2    -> do (S.TypeArrow tau11 tau12) <- typing capGamma t1
                       tau <- typing capGamma t2
                       if tau == tau11 then Just tau12 else Nothing
  S.If t1 t2 t3  -> do S.TypeBool <- typing capGamma t1
                       t <- typing capGamma t2
                       tau <- typing capGamma t3
                       if tau == t then Just t else Nothing
  S.IntConst _  -> Just S.TypeInt
  S.BoolConst _ -> Just S.TypeBool
  S.PrimFunc n t f p
      | length t /= (1 + length p) -> Nothing
      | otherwise -> do pt <- mapM (typing capGamma) p
                        case and $ zipWith (==) (init t) pt of
                          True -> Just $ last t
                          False -> Nothing

typeCheck :: S.Term -> S.Type
typeCheck t =
  case typing Empty t of
    Just t -> t
    _ -> error "type error"  

