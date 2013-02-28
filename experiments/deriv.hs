module Deriv where
import Control.Applicative

data D a = D a a
{-
instance Functor ((->) t) where
  fmap f g = f . g

instance Applicative ((->) t) where
  pure = const
  f <*> g = \t -> (f t) (g t)


instance Num b => Num (a -> b) where
  fromInteger = pure . fromInteger
  a + b = (+) <$> a <*> b -- (fmap (+) a) <*> b
-}



-- toD :: (a -> a) -> (a -> D a)
-- toD f = D <$> f <*> d f

