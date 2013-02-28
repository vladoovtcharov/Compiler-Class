import Control.Monad
newtype Parser s t = P ( [s] -> [(t, [s])] )
unP (P p) = p

pSym :: Eq s => s -> Parser s s
pSym a = P (\inp ->case inp of
                   (s: ss) | x==a -> [(s, ss)]
                   otherwise -> []
           )
pReturn :: a -> Parser s a
pReturn a = P (\inp -> [(a, inp)])

pFail = P (const[])



(<*>) :: Parser s (b->a) -> Praser s b -> Parser s a
(<*>) = ap


