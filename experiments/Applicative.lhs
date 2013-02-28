\begin{code}
{-# LANGUAGE RankNTypes, GADTs, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, TypeOperators #-}
module Applicative where
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
\end{code}

`Describes` captures the relation between input tokens and temrinal symbols

\begin{code}
class symbol `Describes` token where
  eqSymTok :: symbol -> token -> Bool
\end{code}

`Symbol` implments a desription, so given a terminal symbol
it returns a parser which returns a witness token.

\begin{code}
class Symbol p symbol token where
  pSym :: symbol -> p token
\end{code}

`Provides` generalizatyion of the above, so we don't have
to have the input be tokes, what we care about is - 
the input state can provide a token on demn

\begin{code}
class Provides state symbol token | state symbol -> token where
  splitState :: symbol -> state -> Maybe (token, state)
\end{code}
owe need to check whether to keep accepting input or if the end of hte file has been reached.
\begin{code}
class Eof state where
  eof :: state -> Bool
\end{code}

Since thre iwll be different interfaces we have a parser function
which knows how to work with a specific parser and return the result
\begin{code}
class Parser p where
  parse :: p state a -> state -> a
\end{code}

we now implement four parser.  These will take a state as input, rather then a list of symbols.
Accordingly they are polymorphic in the state and witness type, 
where as the previous were polymorphic in the symbol and witness type

the state mut be an instance of Provides and have a symbol and token type which are instances of Describes

also the parser will return a signle result, and unlike the previous ambious parsers

finally the result isn't a witness and state pair, but a witness in a steps datatype.

lazy evaluation provides nice way to drive all the active possibilities in a step by step fashion (breath first)
- a lazily constructed trace of the parsing progress?
\begin{code}
data Steps a where
  Step :: Steps a -> Steps a
  Fail ::            Steps a
  Done :: a       -> Steps a
\end{code}

to allow for different parsers, we need some way of tying there steps together, we do this witht he best function
\begin{code}
best :: Steps a -> Steps a -> Steps a
Fail     `best` r        = r
l        `best` Fail     = l
(Step l) `best` (Step r) = Step (l `best` r)
_        `best` _        = error "incorrect parser"
\end{code}

Recognizer
\begin{code}
newtype R st a = R (forall r . (st -> Steps r) -> st -> Steps r)
unR (R p) = p

instance Applicative (R st) where
  R p <*> R q = R (\k st -> p (q k) st)
  R p <|> R q = R (\k st -> p k st `best` q k st)
  f   <$> R p = R p
  pReturn a   = R (\k st -> k st)
  pFail       = R (\k st -> Fail)

instance (symbol `Describes` token, Provides state symbol token) 
      => Symbol (R state) symbol token where
      pSym a = R (\k st -> case splitState a st of
                             Just (t, ss) -> if a `eqSymTok` t
                                             then Step (k ss)
                                             else Fail
                             Nothing      -> Fail
                 )
\end{code}
