module SimpleParser where

newtype Parser s t = P([s] -> [(t,[s])])

pReturn :: t -> Parser s t
pReturn t = P (\s -> [(t,s)])

pFail :: Parser s t
pFail = P (\_ -> [])

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
P p1 <*> P p2 = P (\s -> [(f a, s'') | (f,s') <- p1 s, (a, s'') <- p2 s'])

(<|>) :: Parser s a -> Parser s a -> Parser s a
P p1 <|> P p2 = P (\s -> p1 s ++ p2 s)

(<$>) :: (a -> b) -> (Parser s a -> Parser s b)
f <$> p1 = pReturn f <*> p1
