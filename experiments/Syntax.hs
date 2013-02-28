{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}
module Syntax where
import Control.Monad

class Applicative p where
  (<*>)   :: p (b -> a) -> p b -> p a 
  (<$>)   :: (b -> a)   -> p b -> p a
  (<|>)   :: p a        -> p a -> p a
  pReturn :: a                 -> p a
  pFail   ::                      p a
  f <$> q = pReturn f <*> q

instance Applicative p => Functor p where
  fmap = (<$>)

class symbol `Describes` token where
  eqSymTok :: symbol -> token -> Bool

class Symbol p symbol token where
  pSym :: symbol -> p token

class Provides state symbol token | state symbol -> token where
  splitState :: symbol -> state -> Maybe (token, state)

class Eof state where
  eof :: state -> Bool

class Parser p where
  parse :: p state a -> state -> a

data Steps a where
  Step :: Steps a -> Steps a
  Fail ::            Steps a
  Done :: a       -> Steps a

best :: Steps a -> Steps a -> Steps a
Fail     `best` r        = r
l        `best` Fail     = l
(Step l) `best` (Step r) = Step (l `best` r)
_        `best` _        = error "incorrect parser"
