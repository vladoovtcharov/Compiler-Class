\section{Abstract-syntactic preliminaries}
\begin{code}
module AbstractSyntax where

class LatexShow tau where
  latexShow :: tau -> String
\end{code}
\subsection{Source types}
\begin{code}
data Type  =  TypeArrow Type Type
           |  TypeBool
           |  TypeInt
           deriving Eq

instance Show Type where
  show  (TypeArrow tau1 tau2)   =  "->(" ++ show tau1 ++ "," ++ show tau2 ++ ")"
  show  TypeBool                =  "Bool"
  show  TypeInt                 =  "Int"

instance LatexShow Type where
  latexShow  (TypeArrow tau1 tau2)    =  "$\\rightarrow$ (" ++ latexShow tau1
                                         ++ ", " ++ latexShow tau2 ++ ")"
  latexShow  TypeBool                 =  "Bool"
  latexShow  TypeInt                  =  "Int"
\end{code}
\subsection{Source terms}
\begin{code}

type Var  =  String
type IntConst = Integer
type BoolConst = Bool
data BinOp a = BinOp (Term->Term->a) Term Term

data Term  =  Var Var
           |  IntConst IntConst
           |  BoolConst BoolConst
           |  Abs Var Type Term
           |  App Term Term
           |  If Term Term Term
           |  BinOpBool (BinOp BoolConst)
           |  BinOpInt (BinOp IntConst)


instance Show Term where
  show (Var x)         =  show x
  show (IntConst x)    =  show x
  show (BoolConst x)   =  show x
  show (Abs x tau t)   =  "abs(" ++ x ++ ":" ++ show tau ++ "." ++ show t ++ ")"
  show (App t1 t2)     =  "app(" ++ show t1  ++ "," ++ show t2 ++ ")"
  show (If t1 t2 t3)   =  "if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ " fi"
  show (BinOpBool (BinOp f a b))   =  "BinOpBool("++ show a ++ "," ++ show b ++ ")"
  show (BinOpInt (BinOp f a b))   =  "BinOpInt("++ show a ++ "," ++ show b ++ ")"

{-
  show (IntConst t1)   =  show t1
  show (IntAdd t1 t2)  =  show t1 ++ "+" ++ show t2
  show (IntSub t1 t2)  =  show t1 ++ "-" ++ show t2
  show (IntMul t1 t2)  =  show t1 ++ "*" ++ show t2
  show (IntDiv t1 t2)  =  show t1 ++ "/" ++ show t2
  show (IntNand t1 t2) =  show t1 ++ "|" ++ show t2
  show (IntEq t1 t2)   =  show t1 ++ "=" ++ show t2
  show (IntLt t1 t2)   =  show t1 ++ "<" ++ show t2

instance LatexShow Term where
  latexShow  (Var x)            =  x
  latexShow  (Abs x tau t)      =  "$\\lambda$" ++ x ++ ": " ++ latexShow tau
                                   ++ ". " ++ latexShow t
  latexShow  (App t1 t2)        =  "$\\blacktriangleright$ (" ++ latexShow t1  ++ ", " ++ latexShow t2 ++ ")"
  latexShow  Tru                =  "true"
  latexShow  Fls                =  "false"
  latexShow  (If t1 t2 t3)      =  "if " ++ latexShow t1 ++ " then " ++ latexShow t2
                                   ++ " else " ++ latexShow t3 ++ " fi"
  latexShow (IntConst t1)       =  show t1
  latexShow (IntAdd t1 t2)      =  latexShow t1 ++ "+" ++ latexShow t2
  latexShow (IntSub t1 t2)      =  latexShow t1 ++ "-" ++ latexShow t2
  latexShow (IntMul t1 t2)      =  latexShow t1 ++ "*" ++ latexShow t2
  latexShow (IntDiv t1 t2)      =  latexShow t1 ++ "/" ++ latexShow t2
  latexShow (IntNand t1 t2)     =  latexShow t1 ++ "|" ++ latexShow t2
  latexShow (IntEq t1 t2)       =  latexShow t1 ++ "=" ++ latexShow t2
  latexShow (IntLt t1 t2)       =  latexShow t1 ++ "<" ++ latexShow t2
-}
\end{code}

\newpage
\subsection{Binding and free variables}
\begin{code}
fv :: Term -> [Var]
-- list of free variables of a term
fv (BoolConst _)   =  []
fv (IntConst _)    =  []
fv (Var x)         =  [x]
fv (If t1 t2 t3)   =  (fv t1)++(fv t2)++(fv t3)
fv (Abs x _ t1)    =  filter (/=x) (fv t1)
fv (App t1 t2)     =  (fv t1)++(fv t2)
fv (BinOpBool (BinOp f a b)) = (fv a) ++ (fv b)
fv (BinOpInt (BinOp f a b)) = (fv a) ++ (fv b)
\end{code}

\subsection{Substitution}
Substitution: |subst x s t|, or $[x \mapsto s]t$ in Pierce,
is the result of substituting |s| for |x| in |t|.

\begin{code}

subst :: Var -> Term -> Term -> Term

subst x s (Var v)       = if (v == x) then s else (Var v)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Abs y tau term) = if (x == y || elem y (fv s))
                             then Abs y tau term
                             else Abs y tau (subst x s term)

subst x s (App t1 t2)     = App (subst x s t1) (subst x s t2)
subst x s (BinOpBool (BinOp f a b)) = BinOpBool (BinOp f (subst x s a) (subst x s b))
subst x s (BinOpInt (BinOp f a b)) = BinOpInt (BinOp f (subst x s a) (subst x s b))
subst x s z  = z

isValue :: Term -> Bool
isValue (Abs _ _ _)    = True
isValue (BoolConst _ ) = True
isValue (IntConst _)   = True
isValue otherwise      = False

\end{code}
\section{Scanning}
Lexical analysis (scanning) is the process of converting a sequence of characters into a sequence of tokens. A token is a string of characters, categorized according to the rules as a symbol. A data type called |Token| is implemented that defines various keywords of the language.
\begin{code}
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

toInt s = case reads s :: [(Integer, String)] of
  [(x, "")] -> x
  _         -> 0

data Token = Arrow
           | Lpar
           | Rpar
           | Comma
           | BoolKeyword
           | IntKeyword
           | Identifier String
           | AbsKeyword
           | Colon
           | Fullstop
           | AppKeyword
           | TrueKeyword
           | FalseKeyword
           | IfKeyword
           | ThenKeyword
           | ElseKeyword
           | FiKeyword
           | Intliteral Integer
           | Plus
           | Minus
           | Mul
           | Div
           | Nand
           | Equal
           | Lt
           deriving (Eq, Show)

scan :: String -> [Token]
scan s = map makeKeyword (f s)
  where
    f :: [Char] -> [Token]
    f []               =  []
    f (' ':t)          =  f t
    f ('\t':t)         =  f t
    f ('\n':t)         =  f t
    f ('-':'>':t)      =  Arrow : f t
    f ('(':t)          =  Lpar : f t
    f (')':t)          =  Rpar : f t
    f (',':t)          =  Comma : f t
    f (':':t)          =  Colon : f t
    f ('.':t)          =  Fullstop : f t
    f ('+':t)          =  Plus : f t
    f ('-':t)          =  Minus : f t
    f ('*':t)          =  Mul : f t
    f ('/':t)          =  Div : f t
    f ('^':t)          =  Nand : f t
    f ('=':t)          =  Equal : f t
    f ('<':t)          =  Lt : f t
    f (h:t)            =  g [h] t

    g :: [Char] -> [Char] -> [Token]
    g s [] = [Identifier s]
    g s (t:ts) = if (elem t ([' ','\t','\n','-','>','(',')',',',':','.']))
           then ((Identifier s) : (f (t:ts)))
           else (g (s++[t]) ts)

    makeKeyword :: Token -> Token
    makeKeyword (Identifier s)
      |  s == "abs"     =  AbsKeyword
      |  s == "app"     =  AppKeyword
      |  s == "true"    =  TrueKeyword
      |  s == "false"   =  FalseKeyword
      |  s == "if"      =  IfKeyword
      |  s == "then"    =  ThenKeyword
      |  s == "else"    =  ElseKeyword
      |  s == "fi"      =  FiKeyword
      |  s == "Bool"    =  BoolKeyword
      |  s == "Int"     =  IntKeyword
      |  isInteger(s)   =  Intliteral $ toInt s
      |  otherwise      =  Identifier s
    makeKeyword t = t
    
\end{code}
\section{Parsing}
Parsing is the process of analyzing a text, made of a sequence of tokens to determine its grammatical structure with respect to a given (more or less) formal grammar. Here, a data type called |Term| is implemented that defines the grammar of the language.
\begin{code}    

parseType :: [Token] -> Maybe (Type, [Token])
parseType (Arrow:Lpar:t1) =
  do (tau1, Comma:t2) <- parseType t1
     (tau2, Rpar:t3)  <- parseType t2
     Just(TypeArrow tau1 tau2, t3)

parseType (BoolKeyword:t) = Just (TypeBool, t)
parseType (IntKeyword:t)  = Just (TypeInt, t)

parseTerm :: [Token] -> Maybe (Term, [Token])
parseTerm t = case t of
                (Identifier s:t) -> Just (Var s, t)
                (Intliteral x:t) -> Just (IntConst x, t)

                (AbsKeyword:Lpar:t) ->
                    do (Var var, Colon:r1) <- parseTerm t
                       (tau, Fullstop:r2)  <- parseType r1
                       (term, Rpar:r3)     <- parseTerm r2
                       Just ((Abs var tau term), r3)

                (AppKeyword:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((App t1 t2), r2)

                (TrueKeyword:t)  -> Just(Tru, t)
                (FalseKeyword:t) -> Just(Fls, t)

                (IfKeyword:t) ->
                    do (t1, ThenKeyword:rt1) <- parseTerm t
                       (t2, ElseKeyword:rt2) <- parseTerm rt1
                       (t3, FiKeyword:rt3)   <- parseTerm rt2
                       Just((If t1 t2 t3), rt3)

                (Plus:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntAdd t1 t2), r2)

                (Minus:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntSub t1 t2), r2)

                (Mul:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntMul t1 t2), r2)

                (Div:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntDiv t1 t2), r2)

                (Nand:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntNand t1 t2), r2)

                (Equal:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntEq t1 t2), r2)

                (Lt:Lpar:t) ->
                    do (t1, Comma:r1) <- parseTerm t
                       (t2, Rpar:r2)  <- parseTerm r1
                       Just ((IntLt t1 t2), r2)

                otherwise -> Nothing

parse :: [Token] -> Term
parse ts =
  case parseTerm ts of
      Just (t, []) -> t
      Just (t, _)  -> error "Syntax error: spurious input at end"
      Nothing      -> error "Syntax error."


\end{code}
