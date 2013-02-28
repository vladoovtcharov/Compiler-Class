{-# LANGUAGE RankNTypes, FlexibleInstances,  FunctionalDependencies, TypeOperators #-}
module Recognizer where
import Syntax

newtype R st a = R (forall r . (st -> Steps r) -> st -> Steps r)
unR (R p) = p 


instance Applicative (R st) where
  R p <*> R q = R (\k st -> p (q k) st) 
  R p <|> R q = R (\k st -> p k st `best` q k st) 
  f   <$> R p = R p 
  pReturn a   = R (\k st -> k st) 
  pFail       = R (\k st -> Fail)

{-
instance Applicative (R st) where
  R p <*> R q = R (p.q)
  R p <|> R q = R 
  f   <$> R q = R q
  pReturn a   = R id
  pFail       = R (const . (const Fail))
-}

instance (symbol `Describes` token, Provides state symbol token) 
  => Symbol (R state) symbol token where
      
  pSym a = R (\k st -> case splitState a st of
                       Just (t, ss) | a `eqSymTok` t ->  Step (k ss) 
                       otherwise                     -> Fail
             )



data Term = Var String
          | Abs String Term
          | App Term Term


instance String `Describes` Term where
  eqSymTok s t = case (s, t) of
                   (y, Var x) -> y == x
                   ("Abs", Abs _ _) -> True
                   ("App", App _ _) -> True
    
