\section{Abstract-syntactic preliminaries}
We added the following terms to the language:
\begin{align*}
t::= &\\
&...\\
&\mbox{fix }t\\
&\mbox{let } x = t_1 \mbox{ in } t_2 \mbox{ end}
\end{align*}
\subsection{Source types}
\begin{code}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances #-}

module AbstractSyntax where

data Expr f = In (f (Expr f))

-- coproduct 
data (f :+: g) e = Inl (f e) | Inr (g e)


class Functor f => Eval f a where
  evalAlgebra :: f a -> a

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

-- coproduct of two functors is a functor
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr y) = Inr (fmap f y)

-- evaluation of coproduct
instance (Eval f a, Eval g a) => Eval (f :+: g) a where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

-- Deriving subtypes (not complete and has overlaps...
instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj


data Var a = Var String
instance Functor Var where
  fmap f (Var a) = Var a
instance Eval Var  where
  evalAlgebra (Var a) = Var a

{-
data Type  =  TypeArrow Type Type
           |  TypeBool
           |  TypeInt
           deriving Eq
\end{code}
\subsection{Source terms}
\begin{code}
type Var  =  String
type IntConst = Integer
data Term  = Var Var
           | Abs Var Type Term
           | App Term Term
           | Fix Term
           | Let Var Term Term
           | Tru
           | Fls
           | If Term Term Term
           | IntConst IntConst
           | IntAdd Term Term
           | IntSub Term Term
           | IntMul Term Term
           | IntDiv Term Term
           | IntNand Term Term
           | IntEq Term Term
           | IntLt Term Term
           deriving Eq
\end{code}
\subsection{Binding and free variables}
\begin{code}
fv :: Term -> [Var]
-- list of free variables of a term
fv Tru             =  []
fv Fls             =  []
fv (IntConst t1)   =  []
fv (Var x)         =  [x]
fv (If t1 t2 t3)   =  (fv t1)++(fv t2)++(fv t3)
fv (Abs x _ t1)    =  filter (/=x) (fv t1)
fv (Fix t)         =  fv t
fv (Let v t1 t2)   =  filter (/=v) $ (fv t1) ++ (fv t2)
fv (App t1 t2)     =  (fv t1)++(fv t2)
fv (IntAdd t1 t2)  =  (fv t1)++(fv t2)
fv (IntSub t1 t2)  =  (fv t1)++(fv t2)
fv (IntMul t1 t2)  =  (fv t1)++(fv t2)
fv (IntDiv t1 t2)  =  (fv t1)++(fv t2)
fv (IntNand t1 t2) =  (fv t1)++(fv t2)
fv (IntEq t1 t2)   =  (fv t1)++(fv t2)
fv (IntLt t1 t2)   =  (fv t1)++(fv t2)

\end{code}

\subsection{Substitution}
Substitution: |subst x s t|, or $[x \mapsto s]t$ is the result of substituting |s| for |x| in |t|.

\begin{code}
subst :: Var -> Term -> Term -> Term

subst x s Fls = Fls
subst x s Tru = Tru
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Var v)       = if (v == x) then s else (Var v)

subst x s (Abs y tau term) = if (x == y || elem y (fv s))
                             then Abs y tau term
                             else Abs y tau (subst x s term)

subst x s (Fix t)          = Fix (subst x s t)
subst x s (Let y t1 t2)    = if (x == y || elem y (fv s))
                             then Let y (subst x s t1) t2
                             else Let y (subst x s t1) (subst x s t2)

subst x s (App t1 t2)     = App (subst x s t1) (subst x s t2)
subst x s (IntAdd t1 t2)  = IntAdd (subst x s t1) (subst x s t2)
subst x s (IntSub t1 t2)  = IntSub (subst x s t1) (subst x s t2)
subst x s (IntMul t1 t2)  = IntMul (subst x s t1) (subst x s t2)
subst x s (IntDiv t1 t2)  = IntDiv (subst x s t1) (subst x s t2)
subst x s (IntNand t1 t2) = IntNand (subst x s t1) (subst x s t2)
subst x s (IntEq t1 t2)   = IntEq (subst x s t1) (subst x s t2)
subst x s (IntLt t1 t2)   = IntLt (subst x s t1) (subst x s t2)
subst x s z = z

isValue :: Term -> Bool
isValue (Abs _ _ _)  = True
isValue Tru          = True
isValue Fls          = True
isValue (IntConst _) = True
isValue otherwise    = False

\end{code}
\section{Scanning}
\begin{code}

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

toInt s = case reads s :: [(Integer, String)] of
  [(x, "")] -> x
  _         -> 0

data Token =  Arrow
           |  Lpar
           |  Rpar
           |  Comma
           |  BoolKeyword
           |  IntKeyword
           |  Identifier String
           |  AbsKeyword
           |  Colon
           |  Fullstop
           |  AppKeyword
           |  TrueKeyword
           |  FalseKeyword
           |  IfKeyword
           |  ThenKeyword
           |  ElseKeyword
           |  FiKeyword
           |  Intliteral Integer
           |  Plus
           |  Minus
           |  Mul
           |  Div
           |  Nand
           |  Equal
           |  Lt
           |  FixKeyword
           |  LetKeyword
           |  InKeyword
           |  EndKeyword
           deriving (Eq, Show)

scan :: String -> [Token]
scan s = map makeKeyword (f s)
  where
    f :: [Char] -> [Token]
    f []               =  []
    f (' ':t)          =  f t
    f ('\t':t)         =  f t
    f ('\n':t)         =  f t
    f ('\r':t)         =  f t
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
    g s (t:ts) = if (elem t ([' ','\t','\r','\n','-','>','(',')',',',':','.']))
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
      |  s == "fix"     =  FixKeyword
      |  s == "let"     =  LetKeyword
      |  s == "in"      =  InKeyword
      |  s == "end"     =  EndKeyword
      |  isInteger(s)   =  Intliteral $ toInt s
      |  otherwise      =  Identifier s
    makeKeyword t = t

\end{code}
\section{Parsing}
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

                (FixKeyword:Lpar:t) ->
                    do (t1, Rpar:r) <- parseTerm t
                       Just ((Fix t1), r)

                (LetKeyword:t) ->
                    do (Var var, Equal:r1) <- parseTerm t
                       (t1, InKeyword:r2)  <- parseTerm r1
                       (t2, EndKeyword:r3) <- parseTerm r2
                       Just ((Let var t1 t2), r3)

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
-}
\end{code}
