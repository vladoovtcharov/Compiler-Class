{-# LANGUAGE MultiParamTypeClasses, GADTs, KindSignatures, TypeFamilies #-}
data Lam :: * -> * where
  Var :: String                       -> Lam String
  IntConst :: Integer                 -> Lam Integer
  BoolConst :: Bool                   -> Lam Bool
  Op :: ([Lam a] -> Lam b) -> [Lam a] -> Lam b

data Tree a = a | a (Node a) (Node a)

type family Der (f :: (* -> *)) :: (* -> *)

type instance Der ( 
