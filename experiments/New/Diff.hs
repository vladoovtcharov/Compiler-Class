module Diff where
import qualified Data.IntMap as IM
data Diff a = C a 
            | D a (IM.IntMap (Diff a))
              deriving (Eq,Ord,Show)

dVar x id = D x $ IM.fromList [(id,C 1)]

instance Functor Diff where
  fmap f (C a) = C (f a)
  fmap f (D a set) = D (f a) (IM.map (fmap f) set)

add (C x) (C y) = C (x + y)
add (C x) (D y vars) = D (x+y) vars
add (D x vars) (C y) = D (x+y) vars
add (D x vx) (D y vy) = D (x+y) (IM.unionWith (+) vx vy)


