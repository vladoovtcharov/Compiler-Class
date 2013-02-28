{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- module TypesCarte where
-- First attemp

data Expr_a = Val_a Int | Add_a Expr_a Expr_a

eval_a :: Expr_a -> Int
eval_a (Val_a x) = x
eval_a (Add_a x y) = eval_a x + eval_a y

example_a :: Expr_a
example_a = Add_a 
              (Add_a 
                (Val_a 3) 
                (Val_a 5)
              ) 
              (Val_a 6)


data Expr f = In (f (Expr f))

data Val e = Val Int
type IntExpr = Expr Val

data Add e = Add e e
type AddExpr = Expr Add


-- co product (sum?)
data (f :+: g) e = Inl (f e) | Inr (g e)

example_b :: Expr (Val :+: Add)
example_b = In (Inr (Add 
              (In (Inr (Add
                (In (Inl (Val 3))) 
                (In (Inl (Val 5)))
              )))
              (In (Inl (Val 6)))
            ))



-- why do we igno f here?  kinda confused at this point
instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)

-- coproduct of two functors is a functor
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr y) = Inr (fmap f y)

-- then given a functor we can fold over its expressions
-- the first argument is called an algbra
-- this looks like a depth first evaluator ... 
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr



-- now adding nice notation

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

(<+>) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

infixl 6 <+> 


example_c = (val 3 <+> val 5) <+> val 6 :: Expr (Add :+: Val)
