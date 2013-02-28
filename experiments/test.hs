
class Term a where
  --parse :: string -> a
  fv :: a -> [String]
  fv x = []
  --subs :: Term b => String -> b -> a -> a
  --isValue :: a -> Bool

class (Term a, Term b) => Eval a b where
  eval :: a -> b

class Type a 

data Var = Var String
instance Term Var where
  fv (Var v) = [v]
instance Eval Var Var where
  eval = id

data Term a => App a = App a a 
instance Term a =>Term (App a) where
  fv (App t1 t2) = (fv t1) ++ (fv t2)

instance (Term ) => Eval (App a) (a) where
  eval (App x y) = x
 

data TypeBool = TypeBool
instance Type TypeBool

data (Term a, Type b) => Abs a b = Abs a b a
instance (Term a, Type b) => Term (Abs a b) where
  fv (Abs t1 _ t2) = (fv t1) ++ (fv t2)


