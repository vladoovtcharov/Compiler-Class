module AbstractSyntax where

-- our goal here is to define an abstract syntax 
-- for an extension of simply typed lambda calculus
-- the primitive types we will use are: Integers and Booleans
-- the extensions are the primitive operators: If, Add, Subtract, Multiply, divide, nand, equal, less than


data Type  =  TypeArrow Type Type
           |  TypeBool
           |  TypeInt
           deriving Eq


type Var  =  String
type IntConst = Integer
type BoolConst = Bool

data Term  = Var Var
           | Abs Var Type Term
           | App Term Term
           | IntConst IntConst
           | BoolConst BoolConst
           | If Term Term Term
           | IntAdd Term Term
           | IntSub Term Term
           | IntMul Term Term
           | IntDiv Term Term
           | IntNand Term Term
           | IntEq Term Term
           | IntLt Term Term
           deriving Eq

-- contrast this to a dfeinition of untyped lambda calculus

--  data UntypedLambda = Abs Var UntypedLambda
--                     | App UntypedLambda UntypedLambda


-- finally we add some pretty printing to our abstract language
instance Show Type where
  show  (TypeArrow tau1 tau2)   =  "(" ++ show tau1 ++ " -> " ++ show tau2 ++ ")"
  show  TypeBool                =  "Bool"
  show  TypeInt                 =  "Int"

instance Show Term where
  show (Var x)         =  show x
  -- show (IntConst x)    =  show x
  show (BoolConst x)   =  show x
  show (Abs x tau t)   =  "abs(" ++ x ++ ":" ++ show tau ++ ". " ++ show t ++ ")"
  show (App t1 t2)     =  "app(" ++ show t1  ++ ", " ++ show t2 ++ ")"
  show (If t1 t2 t3)   =  "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
  show (IntConst t1)   =  show t1
  show (IntAdd t1 t2)  =  "(" ++ show t1 ++ " + " ++ show t2 ++ ")"
  show (IntSub t1 t2)  =  "(" ++ show t1 ++ " - " ++ show t2 ++ ")"
  show (IntMul t1 t2)  =  "(" ++ show t1 ++ " * " ++ show t2 ++ ")"
  show (IntDiv t1 t2)  =  "(" ++ show t1 ++ " / " ++ show t2 ++ ")"
  show (IntNand t1 t2) =  "(" ++ show t1 ++ " | " ++ show t2 ++ ")"
  show (IntEq t1 t2)   =  "(" ++ show t1 ++ " = " ++ show t2 ++ ")"
  show (IntLt t1 t2)   =  "(" ++ show t1 ++ " < " ++ show t2 ++ ")"

-- looking at the definitions above it looks like we are repeating ourselves 
-- when we write the definition of the different binary operations
-- later on we will look at different methods around this (shallow embeding - hoas etc.)
