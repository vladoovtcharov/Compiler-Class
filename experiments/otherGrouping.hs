module LambdaSynatx where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data Type = TypeArrow Type Type
      <|> (pBinType (TypeArrow) "->")
          | TypeBool
pType  =  (TypeBool <$ reserved "Bool")
          | TypeInt
     <|> (TypeInt  <$ reserved "Int")
          deriving (Eq, Show)
         
type Var = String
type IntConst = Integer
data BoolConst = Tru | Fls deriving (Eq, Show)

data Term = 
      Var  :: String,
      Parser :: <$> identifier)
      FreeVars  (Var x)  = [x]


          | Abs       Var Type Term
      <|> (pAbs)
pAbs = Abs <$ reserved "abs" 
           <* symbol "(" <*> pVar
           <* symbol ":" <*> pType
           <* symbol "." <*> pTerm
           <* symbol ")"
fv (Abs x _ t1)    = filter (/=x) (fv t1)

          | App       Term Term
      <|> (pBinTerm (App)     "app")
fv (App t1 t2)     = binFv t1 t2

          | BoolConst BoolConst
=  (BoolConst Tru    <$  reserved "true")
      <|> (BoolConst Fls    <$  reserved "false")

fv (BoolConst _)    = []

          | IntConst  IntConst
      <|> (IntConst           <$> integer)
fv (IntConst _)     = []
          | If        Term Term Term
      <|> (pIf)
pIf = If <$ reserved "if"   <*> pTerm
         <* reserved "then" <*> pTerm
         <* reserved "else" <*> pTerm
         <* reserved "fi"
fv (If t1 t2 t3)   = (fv t1) ++ (fv t2) ++ (fv t3)

          | IntAdd    Term Term
      <|> (pBinTerm (IntAdd)  "+")
fv (IntAdd t1 t2)  = binFv t1 t2
          | IntSub    Term Term
      <|> (pBinTerm (IntSub)  "-")
fv (IntSub t1 t2)  = binFv t1 t2 
          | IntMul    Term Term
      <|> (pBinTerm (IntMul)  "*")
fv (IntMul t1 t2)  = binFv t1 t2
          | IntDiv    Term Term
      <|> (pBinTerm (IntDiv)  "/")
fv (IntDiv t1 t2)  = binFv t1 t2
          | IntNand   Term Term
      <|> (pBinTerm (IntNand) "~")
fv (IntNand t1 t2) = binFv t1 t2
          | IntEq     Term Term
      <|> (pBinTerm (IntEq)   "=")
fv (IntEq t1 t2)   = binFv t1 t2
          | IntLt     Term Term
      <|> (pBinTerm (IntLt)   "<")
fv (IntLt t1 t2)   = binFv t1 t2
          deriving (Eq, Show)




binFv :: Term -> Term -> [Var]
binFv t1 t2 = (fv t1) ++ (fv t2)



isValue :: Term -> Bool
isValue (Abs _ _ _)   = True
isValue (BoolConst _) = True
isValue (IntConst _)  = True
isValue otherwise     = False




pBin      :: CharParser () a -> (a -> a -> a) -> String -> CharParser () a
pBin p t s = t <$ reserved s
               <* symbol "(" <*> p
               <* symbol "," <*> p
               <* symbol ")"

pBinTerm = pBin pTerm
pBinType = pBin pType




pExpr = id <$ whiteSpace <*> pTerm <* eof 

run :: String -> IO ()
run input
        = case (parse pExpr "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x





---------------------------------------------------------------------

pure   :: a -> CharParser () a
pure    = return

(<*>)  :: CharParser () (a -> b) -> CharParser () a -> CharParser () b
(<*>)   = ap

(*>)   :: CharParser () a -> CharParser () b -> CharParser () b
(*>)    = (>>)

(<*)   :: CharParser () a -> CharParser () b -> CharParser () a
m <* n  = do x <- m; n; return x

empty  :: CharParser () a
empty   = mzero

(<$>)  :: (a -> b) -> CharParser () a -> CharParser () b
(<$>)   = fmap

(<$)   :: a -> CharParser () b -> CharParser () a
x <$ m  = m >> return x

