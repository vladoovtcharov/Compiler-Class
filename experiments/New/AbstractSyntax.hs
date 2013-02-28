{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
module AbstractSyntax where
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token
import IntegerArithmetic as A

class LatexShow a where
  latexShow :: a -> String
{-
data TermError = NumArgs Integer [Term]
               | TypeMismatch String Term
               | Parser ParseError
               | UnboundVar String String
               | Default String
showError :: TermError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
showError (Parser err) = show err
instance Error TermError where
  noMsg  = Default "An error has occured"
  strMsg = Default
instance Show TermError where
  show = showError

type ThrowsError = Either TermError
-}
---------------------------------------------------------------------

data Type = TypeArrow Type Type
          | TypeBool
          | TypeInt
          deriving Eq
instance Show Type where
  show (TypeArrow tau1 tau2) = "(" ++ show tau1 ++ "->" ++ show tau2 ++ ")" 
  show TypeBool = "Bool"
  show TypeInt  = "Int"
instance LatexShow Type where
  latexShow (TypeArrow tau1 tau2) = "$\\rightarrow$ (" ++ latexShow tau1
                                    ++ ", " ++ latexShow tau2 ++ ")" 
  latexShow TypeBool              = "Bool"
  latexShow TypeInt               = "Int"


class (IsValue a, FreeVars a, ParseString a) => Term a 
  --parse :: String -> Maybe a
  --parse s = Nothing
  --freeVars :: a -> [String]
  --freeVars s = []
  -- isValue :: a -> Bool
  -- isValue s= False
  -- subst :: (Term b, Term c, Term d) =>  b -> c  -> a -> d
  -- subst :: a -> a -> a -> a
  -- eval :: a -> a

class IsValue a where
  isValue :: a -> Bool
  isValue _ = False

class FreeVars a where
  freeVars :: a -> [String]
  freeVars _ = []

class ParseString a where
  parseString :: String -> Maybe a
  parseString _ = Nothing
  
data Var where 
  Var :: String -> Var
data Abs where
  Abs :: (Term b) => Var -> Type -> b -> Abs
data App where
  App :: (Term a, Term b) => a -> b -> App
data Fix where
  Fix :: (Term a) => a -> Fix
data Let where
  Let :: (Term a, Term b) => Var -> a -> b -> Let
data If where
  If :: (Term a, Term b, Term c) => a -> b -> c -> If
data IntConst where
  IntConst :: Integer -> IntConst
data BoolConst where
  Tru :: BoolConst
  Fls :: BoolConst


--data PrimFunc a where
--  PrimFunc :: Term a => [a] 


instance FreeVars Var where
  freeVars (Var v) = [v]
instance IsValue Var where
  isValue _ = True
instance ParseString Var where
  parseString s = return $ Var s

instance FreeVars Abs where
  freeVars (Abs v tau t) = freeVars v
instance IsValue Abs where
  isValue _ = True
instance ParseString Abs where
  parseString s = Nothing

instance Term Var
instance Term Abs 
 
parseExpr :: (Term a, Term b) => a -> Maybe b
{-
data LTerm  a where 
  Var :: String -> LTerm String
  App :: LTerm a -> LTerm a -> LTerm a
  Abs :: LTerm String -> Type -> LTerm a -> LTerm a        
  Fix :: LTerm a -> LTerm a
  Let :: LTerm a -> LTerm a -> LTerm a -> LTerm a
  If  :: LTerm a -> LTerm a -> LTerm a -> LTerm a
  IntConst :: Integer -> LTerm Integer
  BoolConst :: Bool -> LTerm Bool
  PrimFunc' :: [LTerm a] -> ([LTerm a] -> LTerm a) -> LTerm a
  PrimFunc ::
    { name :: String
    , funcType :: [Type]
    , funcApp :: [LTerm a] -> LTerm a
    , funcArgs :: [LTerm a]
    } -> LTerm a

instance Term (LTerm a) where 
  freeVars (Var x) = [x]
  freeVars (Abs (Var x) _ t1) = filter (/=x) (freeVars t1)
  freeVars (App t1 t2) = (freeVars t1) ++ (freeVars t2)
  freeVars (Fix x) = freeVars x
  freeVars (Let (Var v) t1 t2) = filter (/=v) $ (freeVars t1) ++ (freeVars t2)
  freeVars (BoolConst _) = []
  freeVars (IntConst _) = []
  freeVars (If t1 t2 t3) = (freeVars t1) ++ (freeVars t2) ++ (freeVars t3)
  freeVars (PrimFunc _ _ _ params) = foldl1 (++) (map freeVars params)

  isValue (Abs _ _ _)   = True
  isValue (Fix _)       = True
  isValue (BoolConst _) = True
  isValue (IntConst _)  = True
  isValue otherwise     = False

subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3) 
subst x s (Var v) = if (v == x) then s else (Var v)
subst x s t@(Abs (Var y) tau t1) 
  | (x == y || elem y (freeVars s)) = t
  | otherwise = Abs (Var y) tau (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)
subst x s t@(Let (Var y) t1 t2)
  | (x == y || elem y (freeVars s))= t
  | otherwise = Let (Var y) (subst x s t1) (subst x s t2)
subst x s (Fix f) = Fix (subst x s f)
subst x s (PrimFunc n t f params) = PrimFunc n t f (map (subst x s) params)
subst x s z  = z


instance Show (LTerm a) where
  show (Var t) = show t
  show (Abs x tau t)   = "(\\" ++ show x ++":" ++ show tau ++ 
                         "." ++ show t ++ ")"
  show (App t1 t2)     = show t1 ++ "(" ++ show t2 ++ ")"
  show (Fix t)         = "fix(" ++ show t ++ ")"
  show (Let x t1 t2)   = "let " ++ show x ++ " = " ++ show t1 ++
                         "in " ++ show t2 ++
                         "end "
  show (If t1 t2 t3)   = "if " ++ show t1 ++ 
                         "then " ++ show t2 ++
                         "else " ++ show t3 ++
                         "fi"
  show (BoolConst True) = "true "
  show (BoolConst False) = "false "
  show (IntConst t)    = show t ++ " "
  show (PrimFunc name _ _ params) = "<"++name++">" ++  show params

-}
-------------------------------------------------------
{-
languageDef = emptyDef 
  { Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   = [ "if", "then", "else", "fi"
                            , "true", "false"
                            , "abs", "app"
                            , "fix", "let", "in", "end"
                            , "Bool", "Int"
                            , "+", "-", "*", "/", "~", "=", "<", "->"
                            ]
  }

lexer      = Token.makeTokenParser languageDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
symbol     = Token.symbol     lexer
-}
-------------------------------------------------------
{-
pType :: Parser Type
pType  =  (TypeBool <$ reserved "Bool")
      <|> (TypeInt  <$ reserved "Int")
      <|> (TypeArrow <$ reserved "->" 
                     <* symbol "(" <*> pType 
                     <* symbol "," <*> pType
                     <* symbol ")")


-- pTerm  = choice $ [ pAbs, pApp, pFix, pLet, pIf] ++ --, pBool ] ++
--                  [ (IntConst <$> integer), (Var <$> identifier) ]
--                  -- typedPrimitiveParsers ++ 

pAbs :: Parser (LTerm a)
pAbs  = Abs <$ reserved "abs"
            <* symbol "(" <*> pVar
            <* symbol ":" <*> pType
            <* symbol "." <*> pTerm
            <* symbol ")" 

pApp :: Parser (LTerm a)
pApp  = App <$ reserved "app"
            <* symbol "(" <*> pTerm
            <* symbol "," <*> pTerm
            <* symbol ")" 

pFix = Fix <$ reserved "fix"
           <* symbol "(" <*> pTerm
           <* symbol ")"

pLet = Let <$ reserved "let"
           <*> pVar <* symbol "=" <*> pTerm
           <* reserved "in" <*> pTerm
           <* reserved "end"

pIf = If <$ reserved "if"   <*> pTerm
         <* reserved "then" <*> pTerm
         <* reserved "else" <*> pTerm
         <* reserved "fi"

pBool =  (BoolConst True <$ reserved "true")
     <|> (BoolConst False <$ reserved "false")

pVar = Var <$> identifier

-}
-------------------------------------------------------
{-
-- type TypedPrimitive = (String, [Type], ([Term] -> ThrowsError Term))
type TypedPrimitive = (String, [Type], ([Term] -> Term))
typedPrimitives :: [TypedPrimitive]
typedPrimitives = [ ("+", [TypeInt, TypeInt, TypeInt], intBinOp A.intAdd)
                  , ("-", [TypeInt, TypeInt, TypeInt], intBinOp A.intSub)
                  , ("*", [TypeInt, TypeInt, TypeInt], intBinOp A.intMul)
                  , ("/", [TypeInt, TypeInt, TypeInt], intBinOp A.intDiv)
                  , ("~", [TypeInt, TypeInt, TypeInt], intBinOp A.intNand)
                  , ("=", [TypeInt, TypeInt, TypeBool], intBinRel A.intEq)
                  , ("<", [TypeInt, TypeInt, TypeBool], intBinRel A.intLt)
                  ]

typedPrimitiveParser :: TypedPrimitive -> Parser Term
typedPrimitiveParser (n, t, f) =  PrimFunc n t f
                              <$  reserved n
                              <*  symbol "("
                              <*> sepBy pTerm (symbol ",")
                              <*  symbol ")"

typedPrimitiveParsers = map typedPrimitiveParser typedPrimitives

intBinOp :: (Integer -> Integer -> Integer) -> [Term] -> Term
intBinOp op ((IntConst t1):(IntConst t2):[]) = IntConst $ op t1 t2

intBinRel :: (Integer -> Integer -> Bool) -> [Term] -> Term
intBinRel op (IntConst t1:IntConst t2:[]) = case op t1 t2 of
                                              True -> BoolConst Tru
                                              False -> BoolConst Fls
-}
-------------------------------------------------------
{-
fv :: Term -> [Var]
fv (Var x) = [x]
fv (Abs x _ t1) = filter (/=x) (fv t1)
fv (App t1 t2) = (fv t1) ++ (fv t2)
fv (Fix x) = fv x
fv (Let v t1 t2) = filter (/=v) $ (fv t1) ++ (fv t2)
fv (BoolConst _) = []
fv (IntConst _) = []
fv (If t1 t2 t3) = (fv t1) ++ (fv t2) ++ (fv t3)
fv (PrimFunc _ _ _ params) = foldl1 (++) (map fv params)

isValue :: Term -> Bool
isValue (Abs _ _ _)   = True
isValue (Fix _)       = True
isValue (BoolConst _) = True
isValue (IntConst _)  = True
isValue otherwise     = False

subst :: Var -> Term -> Term -> Term
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3) 
subst x s (Var v) = if (v == x) then s else (Var v)
subst x s t@(Abs y tau t1) 
  | (x == y || elem y (fv s)) = t
  | otherwise = Abs y tau (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)
subst x s t@(Let y t1 t2)
  | (x == y || elem y (fv s))= t
  | otherwise = Let y (subst x s t1) (subst x s t2)
subst x s (Fix f) = Fix (subst x s f)
subst x s (PrimFunc n t f params) = PrimFunc n t f (map (subst x s) params)
subst x s z  = z
-}
-------------------------------------------------------

--readTerm input = parse pTerm "" input
--readOrThrow :: Parser a -> String -> ThrowsError a
--readOrThrow parser input = case parse parser "" input of
--  Left err -> throwError $ Parser err
--  Right val -> return val

--readTerm = readOrThrow pTerm

-------------------------------------------------------
------------- make the parser applicative -------------
-------------------------------------------------------

pure   :: a -> Parser a
pure    = return

(<*>)  :: Parser (a -> b) -> Parser a -> Parser b
(<*>)   = ap

(*>)   :: Parser a -> Parser b -> Parser b
(*>)    = (>>)

(<*)   :: Parser a -> Parser b -> Parser a
m <* n  = do x <- m; n; return x

empty  :: Parser a
empty   = mzero

(<$>)  :: (a -> b) -> Parser a -> Parser b
(<$>)   = fmap

(<$)   :: a -> Parser b -> Parser a
x <$ m  = m >> return x
