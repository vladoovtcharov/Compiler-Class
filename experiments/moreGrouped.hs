module LambdaSynatx where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


data Type = TypeArrow Type Type
          | TypeBool
          | TypeInt
          deriving (Eq, Show)
         
type Var = String
type IntConst = Integer
data BoolConst = Tru | Fls deriving (Eq, Show)

class BinTerm a where
  dataType :: Term -> Term -> a

data BinOp = IntAdd    Term Term
           | IntSub    Term Term
           | IntMul    Term Term
           | IntDiv    Term Term
           | IntNand   Term Term
           | App       Term Term
           deriving (Eq, Show)

data IntFac = IntFac Term Term
instance BinTerm IntFac  where
  dataType  = IntFac


data BinRel = IntLt Term Term
            | IntEq Term Term
            deriving (Eq, Show)

data Term = Var       Var
          | Abs       Var Type Term
          | BoolConst BoolConst
          | IntConst  IntConst
          | If        Term Term Term
   --       | BinOp  BinOp
          | BinRel BinRel
          deriving (Eq, Show)

{-
languageDef = 
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if", "then", "else", "fi"
                                     , "true", "false"
                                     , "abs", "app"
                                     , "Bool", "Int"
                                     , "+", "-", "*" ,"/" 
                                     , "~", "=", "<" ,"->"
                                     ]
           }

lexer      = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol     lexer







pTerm :: CharParser () Term
pTerm  =  (BoolConst Tru    <$  reserved "true")
      <|> (BoolConst Fls    <$  reserved "false")
      <|> (pIf)
      <|> (pAbs)
--      <|> (BinOp  <$> pBinOp)
      <|> (BinRel <$> pBinRel)
      <|> (IntConst           <$> integer)
      <|> (Var                <$> pVar)
      <?> "Lambda Term"

pType :: CharParser () Type
pType  =  (TypeBool <$ reserved "Bool")
      <|> (TypeInt  <$ reserved "Int")
      <|> (pBinType (TypeArrow) "->")
      <?> "Lambda Type"


pVar :: CharParser () Var
pVar = identifier

pAbs = Abs <$ reserved "abs" 
           <* symbol "(" <*> pVar
           <* symbol ":" <*> pType
           <* symbol "." <*> pTerm
           <* symbol ")"

pIf = If <$ reserved "if"   <*> pTerm
         <* reserved "then" <*> pTerm
         <* reserved "else" <*> pTerm
         <* reserved "fi"

-- Parsing Binary Operators
pBinOp :: CharParser () BinOp
pBinOp =  (pBinTerm (IntAdd)  "+")
      <|> (pBinTerm (IntSub)  "-")
      <|> (pBinTerm (IntMul)  "*")
      <|> (pBinTerm (IntDiv)  "/")
      <|> (pBinTerm (IntNand) "~")
      <|> (pBinTerm (App) "app")
-- Parsing Binary Relation
pBinRel :: CharParser () BinRel
pBinRel =  (pBinTerm (IntEq)   "=")
       <|> (pBinTerm (IntLt)   "<")

-- Helps generate parsers for binary functions of the form "op(a1<-p, a2<-p)"
-- parser -> return type of b a a -> op string -> parser for b
pBin      :: CharParser () a -> (a -> a -> b) -> String -> CharParser () b
pBin p t s = t <$ reserved s
               <* symbol "(" <*> p
               <* symbol "," <*> p
               <* symbol ")"
-- the only nested parsing we use is either nested terms or types 
-- so we eliminate the first var of pBin for convinience
pBinTerm = pBin pTerm
pBinType = pBin pType


-- the main parse, we trim the initial whitespace and then
-- parse for a single term
pExpr = id <$ whiteSpace <*> pTerm <* eof 

run :: String -> IO ()
run input
        = case (parse pExpr "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x





binFv :: Term -> Term -> [Var]
binFv t1 t2 = (fv t1) ++ (fv t2)

fv :: Term -> [Var] 
fv (Var x)         = [x]
fv (Abs x _ t1)    = filter (/=x) (fv t1)
fv (BoolConst _)    = []
fv (IntConst _)     = []
fv (If t1 t2 t3)   = (fv t1) ++ (fv t2) ++ (fv t3)
--fv (App t1 t2)     = binFv t1 t2

isValue :: Term -> Bool
isValue (Abs _ _ _)   = True
isValue (BoolConst _) = True
isValue (IntConst _)  = True
isValue otherwise     = False


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

-}
