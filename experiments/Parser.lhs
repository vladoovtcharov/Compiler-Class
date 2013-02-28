Notes Following: Combinator Parsing: A Short Tutorial - S. Doaitse Swierstra

The parser takes a list of symbols - [s] 
and returns the abstract repersentation, say a tree t
type Parser s t = [s] -> t

Rather then parsing the whole string at one time,
we can break it up, and only parse some symbols, 
and return the remainder
tpye Parser s t = [s] -> (t, [s])

If we also want to allow the parser to return
a list of possible parser rather then a unqiue parse
type Parser s t = [s] -> [(t, [s])]

\begin{code}
newtype Parser s t = P( [s] -> [(t, [s])] )
unP (P p) = p
\end{code}

some examples of how to use this type
to parse the letter a
pLettera :: Parser Char Char
pLettera = P (\inp -> case inp of
                      (s:ss) | s == 'a' -> [('a',ss)]
                      otherwise         -> []
             )

pSym:: Eq s => s -> Parser s s
pSym a = P (\inp -> case inp of 
                    (s:ss) | x == a -> [(s, ss)]
                    otherwise       -> []
           )

The reason we return (s,ss) instead of (a,ss)
is that we may define two terms to be equal (IntConst _ = BoolConst _ = Const)
if we want to parse them in the same way, but still preserve their types
data Token = Const
           | BoolConst Bool
           | IntConst Integer
           | Ident String
           | Abs
instance Eq Token where
  (BoolConst _) == Const = true
  (IntConst _)  == Const = true

now we can create a function which matches any of the consts, regardless of their type
(in the case bool or int)
pConst = pSym Const

\begin{code}
pReturn a = P (\inp -> [(a, inp)])
pFail = P (const [])
\end{code}

We still want to parse the entire list, so we need to figure out
how to chain these parsers.

