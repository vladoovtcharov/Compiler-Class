module Parse where
import AbstractSyntax
-- next we want to parse a concrete syntax into our abstract syntax
-- there is much to say about the theory of parsing, but we will refrain from doing so right now
-- instead trying to jump into the semantics as quickly as possible
-- so for now we define a verey naive, and kind of ugly parser

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

                (TrueKeyword:t)  -> Just(BoolConst True, t)
                (FalseKeyword:t) -> Just(BoolConst False, t)

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

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

toInt s = case reads s :: [(Integer, String)] of
  [(x, "")] -> x
  _         -> 0

