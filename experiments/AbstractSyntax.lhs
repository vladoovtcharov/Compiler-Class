
\documentclass[letterpaper]{article}
%% Literate Haskell script intended for lhs2TeX.

%include lhs2TeX.fmt
\setlength{\parindent}{0pt}
\title{Notes on: Combinator Parsing: A Short Tutorial - S.Doaitse Swierstra}
\author{Vlado Ovtcharov}
\date{01 Mar 2012}
\begin{document}
\maketitle
\thispagestyle{empty}

\begin{code}
module AbstractSyntax where
-- import Control.Applicative
import Control.Monad
import LatexDefs
\end{code}

\section{Abstract Syntax}

\begin{code}
data Type  = TypeArrow Type Type
           | TypeBool
           | TypeInt
           deriving (Eq)

type Var      = String
type IntConst = Integer
data Term = 
            -- lambda-calculus terms
            Var       Var
          | Abs       Var Type Term
          | App       Term Term
            -- extensions (lazy if, general recursion, let binding)
          | If        Term Term Term
          | Fix       Term
          | Let       Var Term Term
            -- constants
          | Tru
          | Fls
          | IntConst  IntConst
            -- functions
          | PrimFunc [Type] [Term]
          | Funcs    [Type] [Term]
          deriving (Eq)

-- we bind our primitive functions
add = PrimFunc [TypeInt, TypeInt, TypeInt]
\end{code}

\section{Printing}
We define latex translations of out Types and Terms.

\begin{code}
instance Show Type where
  show (TypeArrow tau1 tau2) = "(" ++ show tau1 ++ " ->" ++ show tau2 ++ ")"
  show TypeBool = "(Bool)"
  show TypeInt  = "(Int)"
\end{code}


\section{Parser}
We use a combinator type parser, following the paper:
Combinator Parsing: A Short Tutorial - S. Doaitse Swierstra


The parser takes a list of symbols - $[s]$
and returns the abstract repersentation, say a tree t.
\begin{spec}
type Parser s t = [s] -> t
\end{spec}

Rather then parsing the whole string at one time,
we can break it up, and only parse some symbols, 
and return the remainder.
\begin{spec}
tpye Parser s t = [s] -> (t, [s])
\end{spec}

If we also want to allow the parser to return
a list of possible parses rather then a unqiue parse,
we have it return a list of parses.
\begin{spec}
type Parser s t = [s] -> [(t, [s])]
\end{spec}

We then come to the final definition
\begin{code}
newtype Parser s t = P( [s] -> [(t, [s])] )
unP (P p) = p
\end{code}

some examples of how to use this type
to parse the letter a.
\begin{spec}
pLettera :: Parser Char Char
pLettera = P (\inp -> case inp of
                      (s:ss) | s == 'a' -> [('a',ss)]
                      otherwise         -> []
             )
\end{spec}

More generally how to parse a symbol
\begin{spec}
pSym:: Eq s => s -> Parser s s
pSym a = P (\inp -> case inp of 
                    (s:ss) | x == a -> [(s, ss)]
                    otherwise       -> []
           )
\end{spec}

The reason we return (s,ss) instead of (a,ss)
is that we may define two terms to be equal $(IntConst _ == BoolConst _ == Const)$
if we want to parse them in the same way, but still preserve their types
\begin{spec}
data Token = Const
           | BoolConst Bool
           | IntConst Integer
           | Ident String
           | Abs
instance Eq Token where
  (BoolConst _) == Const = true
  (IntConst _)  == Const = true
\end{spec}

now we can create a function which matches any of the consts, regardless of their type
(in the case bool or int)
\begin{spec}
pConst = pSym Const
\end{spec}

Next we define some monad type functions for convinience
(note: applicatives lie between functors and monads,
       later we will make the parser an instance of monad)
\begin{code}
pReturn :: a -> Parser s a
pReturn a = P (\inp -> [(a, inp)])
pFail = P (const [])
\end{code}

Each parse only parses a part of the input, 
but we still want to parse the entire list, 
so we need to figure out how to chain these parsers.

Some ways (and some of their poteential problems)
i.   We could combine them in a list: [Parser s a] 
     but this restricts us to one type of repersentation
     namley whatever the type `a` is.  
     We are only parsing things of type `Term` so this 
     wouldn't be a problem, but sometimes we may want
     to parse more then one repersentation.

ii.  We could chain two parsers at a time 
     (rather then all at once like the list example) 
     and get the type: $Parser s a -> Parser s b -> Parser s (a,b)$
     but then we get deeply nested pairs, which seems a bit awkward.
    
iii. We flipt the previous defintion around a bit, so that
     we start with a complex type and return a simple type
     the result is: Parser s (b -> a) -> Parser s b -> Parser a b
     the idea is if we have a type `(b -> a)` and a type `b` 
     we can apply the former to the latter to get a type `a`

And here's how we define the sequencing operator
\begin{code}
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
P p1 <*> P p2 = P (\inp -> [(v1 v2, ss2) | (v1, ss1) <- p1 inp
                                         , (v2, ss2) <- p2 ss1
                           ]
                  )
\end{code}

At first glance (at least to me) the first argument, 
which has type Parser s (b -> a), seems strange.
In terms of our parser we want to parse things like
App t1 t2, where App has a type Term -> Term -> Term
so this is what will be what start of the sequencing
so for parsing the first term of App
Parser s App -> Parser s Term -> Parser s (Term -> Term)
and then for the second term
Parser s (Term -> Term) -> Parser s Term -> Parser s Term
so we get (in pseudo-code) something like
pReturn App <*> parseTerm <*> parseTerm

Since the pattern: pRetrun func <*> parseArgumnets
will be used often we define an operator for it
\begin{code}
(<$>) :: (b -> a) -> Parser s b -> Parser s a
f <$> p = pReturn f <*> p 
\end{code}

We also add a choice operator,
this combines the output of two parsers.
Note: the pFail is a left and right identity
of the choice operator, so we can also use it
to go down a list of possible parses
\begin{code}
(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P(\inp -> p1 inp ++ p2 inp)
\end{code}

We then define some extra operators for convinence,
which ignore the parameter ont he side where the bracket is missng
\begin{code}
f <$ p = const <$> pReturn f <*> p
p <* q = const <$> p         <*> q
p *> q = id    <$  p         <*> q
\end{code}

And finally we define the precence levels for the operators
\begin{code}
infixl 3 <|>
infixl 5 <*>, <*, *>
infixl 7 <$>, <$
\end{code}

We could now define swapping, chaining and precedence functions 
 <**>, <??>, pChainL, pChainR, pPack
but we will not need them for now so will skip this section

The path now changes, and we generalize the above to allow states,
generalize our parsers, symbols, tokens,
and in general a slight shift in the point of view.

If we wish to add a context to our parser, 
for example the current depth, 
if we are parsing De Bruijn indexes.
So we make our parser an instance of Monad
\begin{code}
instance Monad (Parser s) where
  return = pReturn
  P pa >>= a_pb = P(\inp -> [b_inp | (a, ss1) <- pa inp
                                   , b_inp    <- unP (a_pb a) ss1
                            ]
                   )
\end{code}

We made out parser an instance of Monad,
but originally it layed between functors and monads,
so we create a class wich captures this,
this will also allow us to use it outside the parser
\begin{code}
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


We want to capture the idea of
a function from a symbol to a token
we will want to generalize this perhaps 
and add a state.
We first define the relation between a symbol and token
\begin{code}
class symbol `Describes` token where
  eqSymTok :: symbol -> token -> Bool
\end{code}

Then a definition for statless symbol to token parser
\begin{code}
class Symbol p symbol token where
 pSym :: symbol -> p token
\end{code}

And a statefull symbol to token parser
\begin{code}
class Provides state symbol token | state symbol -> token where
  splitState :: symbol -> state -> Maybe (token, state)
\end{code}

We can generalize our parser to a class
\begin{code}
class Eof state where
  eof :: state -> Bool
class Parser p where
  parse :: p state a -> state -> a
\end{code}

Four different kinds of parser
recognisers
history parsers
future parsers
monad parsers

The parsing is no longer ambiguos and returns a single result
does not return a token and a stae, 
but instead a token/witness wrapped in a Steps datatype.

Our parsers until now have been depth first,
so we have to hang on to the entire input until we are finished
The solution we saw in class was to use continuations.  
Here we take a similiar approach,
but rather then passing continuation as another agument,
we wrap the input in the continuation as a Steps datatype

But in this way we do a breath first search.
\begin{code}
data Steps a where
  Step :: Steps a -> Steps a
  Fail ::            Steps a
  Done :: a       -> Steps a
\end{code}

This works like many of the machines we have already implemented.
We keep taking steps until we are either done with the parsing/computation
or until we get some error.

We then need another function to drive the steps.
\begin{code}
best :: Steps a -> Steps a -> Steps a
Fail     `best` r        = r
l        `best` Fail     = l
(Step l) `best` (Step r) = Step (l `best` r)
_        `best` _        = error "incorrect parser"
\end{code}

If we wanted to allow ambiguity 
we would also pattern match on Done `step` Done

This best function is what changes it from depth to breath first.
if we have to succ

To retrieve the final value of Steps we write an eval function
\begin{code}
eval :: Steps a -> a
eval (Step l) = eval l
eval (Done v) = v
eval Fail     = error "should not happen"
\end{code}

Recognisers
This is called such since it does not return a result,
it merley checks whether it can recognize what it is parsing

We define a new type R,
it has a polymorphic state and witnesses
it takes a state and returns Steps
This Steps starts with the whatever we recognize
and end with the continuation that is passed to the recognizer
\begin{code}
newtype R st a = R ((st -> Steps r) -> st -> Steps r)
unR (R p) = p
\end{code}

The recoginzer doesn't use the witness type `a`.

An implementation of Symbol
if we make a step in the right direction we keep stepping
(the right direction is determined by Describe),
(the stepping is done by provides),
otherwise we fail
\begin{code}
instance (symbol `Describe` s token, Provides state symbol token)
  => Symbol (R state) symbol token where
  pSym a = R (\k h st -> case splitState a st of
                         Just (t, ss) -> if a `eqSymTok` t
                                         then Step (k ss)
                                         else Fail
                         Nothing      -> Fail
             )
\end{code}


History Based
This takes the witnesses and writes them down in the history
so now our parse will take the continuation, the state, and the history
\begin{code}
newtype P_h st a = P_h ((h,a) -> st -> Steps r)
                        -> h  -> st -> Steps r
                       )
unP_h (P_h p) = p
\end{code}

And we implement applicative class
\begin{code}
instance Applicative (P_h state) where
P_h p <*> P_h q = P_h (\k -> p (q apply_h)
                        where apply_h = \h((h, b_a), b) -> k (h, b_a b))
P_h p <|> P_h q = P_h (\k h st -> p k h st `best q k h st)
f     <$> P_h q = P_h (\k      -> p $ \(h, a) -> k (h, f a))
pFail           = P_h (\k _ _  -> Fail)
pReturn a       = P_h (\k h    -> k (h, a))
\end{code}

If we visualize how the data flows we can imagine
our history keeps getting items attached to it,
and when we are done
we back track applying the continuation to Steps
until we get the final result back


We can reformulate the stack so that 
we don't have to keep passing the history around
\begin{code}
P_h st a = P_h (a -> st -> Steps r) -> st -> Steps r
instance Applicative (P_h state) where
  (P_h p) <*> (P_h q) = P_h (\k -> p (\f -> q (\a -> k (f a))))
  (P_h p) <|> (P_h q) = P_h (\k -> inp -> p k inp `est` q k inp)
  f       <$> (P_h p) = P_h (\k -> p (\a -> k (f a)))
  pFail               = P_h (\k -> const noAlts)
  pReturn a           = P_h (\k -> k a)
\end{code}

We define Symbol very close to how we did for the recognizer
\begin{code}
instance (symbol `Describes` token, Provides state symbol token)
  => Symbol (P_h state) symbol token where
  pSym a = P_h (\k st -> case splitState a st of
                         Just (t, ss) -> if a `eqSymTok` t
                                         then Step (k t ss)
                                         else Fail
                         Nothing      -> Fail
               )
\end{code}

We also make it an instance of parser
\begin{code}
insance Eof state => Parser (P_h state) where
  parse (P_h p) = eval.p (\r rs -> if eof rs then Done r else Fail)
\end{code}
\end{document}
