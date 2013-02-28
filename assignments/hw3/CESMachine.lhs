\begin{code}
module CESMachine where
import qualified DeBruijn as S
import qualified IntegerArithmetic as I

data Inst = Int Integer
         |  Bool Bool
         |  Add
         |  Sub
         |  Mul
         |  Div
         |  Nand
         |  Eq
         |  Lt
         |  Access Int
         |  Close Code
         |  Let
         |  EndLet
         |  Apply
         |  Return
         |  If
         |  Fix
         deriving (Show, Eq)

type Code = [Inst]
data Value = BoolVal Bool | IntVal Integer | Clo Code Env | CloFix Code
    deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (IntVal i)  = show i
  show (Clo c e)   = "Clo " ++ show c ++ " " ++ show e
  show (CloFix c)  = "Clo " ++ show c

type Env = [Value]
data Slot = Value Value | Code Code | Env Env
    deriving Show

type Stack = [Slot]
type State = (Code, Env, Stack)

compile :: S.Term -> Code
compile t =
    case t of
      S.Var n         -> [Access n]
      S.Tru           -> [Bool True]
      S.Fls           -> [Bool False]
      S.IntConst i    -> [Int i]
      S.Abs _ t       -> [Close (compile(t) ++ [Return])]
      S.App t1 t2     -> (compile t1) ++ compile(t2)  ++ [Apply]

      S.If t1 t2 t3   -> (compile t1) ++
                         [Close (compile(t2) ++ [Return])] ++
                         [Close (compile(t3) ++ [Return])] ++ [If]

      S.Let t1 t2     -> (compile t1) ++ [Let] ++ (compile t2) ++ [EndLet]
      S.Fix t1        -> (compile t1) ++ [Fix]
      S.IntAdd t1 t2  -> (compile t1) ++ (compile t2) ++ [Add]
      S.IntSub t1 t2  -> (compile t1) ++ (compile t2) ++ [Sub]
      S.IntMul t1 t2  -> (compile t1) ++ (compile t2) ++ [Mul]
      S.IntDiv t1 t2  -> (compile t1) ++ (compile t2) ++ [Div]
      S.IntNand t1 t2 -> (compile t1) ++ (compile t2) ++ [Nand]
      S.IntLt t1 t2   -> (compile t1) ++ (compile t2) ++ [Lt]
      S.IntEq t1 t2   -> (compile t1) ++ (compile t2) ++ [Eq]

step :: State -> Maybe State
step state =
    case state of
      ((Int v):c, e, s)                                      ->
        Just (c, e, (Value (IntVal v)):s)

      ((Bool v):c, e, s)                                     ->
        Just (c, e, (Value (BoolVal v)):s)

      ((Access n):c, e, s)                                   ->
         case e !! n of
           CloFix t  ->  Just (t++c, e, s)
           v         ->  Just (c, e, (Value v):s)

      ((Close c'):c, e, s)                                   ->
         Just (c, e, (Value (Clo c' e)):s)

      (Apply:c, e, (Value v):(Value (Clo c' e')):s)          ->
         Just (c', v:e', (Code c):(Env e):s)

      (Return:c, e, v:(Code c'):(Env e'):s)                  ->
         Just (c', e', v:s)

      (If:c, e, (Value (Clo c3 e3)):(Value (Clo c2 e2)):(Value (BoolVal v)):s)  ->
         if (v == True) then
            Just (c2, e2, (Code c):(Env e):s)
         else
            Just (c3, e3, (Code c):(Env e):s)

      (Let:c, e, (Value v):s)                                ->
         Just (c, v:e, s)

      (EndLet:c, v:e, s)                                     ->
         Just (c, e, s)

      (Fix:c, e, (Value (Clo (Close c':c'') e')) : s)        ->
         Just (c, e, (Value (Clo (c'++c'') ((CloFix (Close ((Close c':c'')):[Fix])):(skipFixEnvs e)))) : s)

      (Add:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)  ->
         Just (c, e, (Value (IntVal (I.intAdd v1 v2)):s))

      (Sub:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)  ->
         Just (c, e, (Value (IntVal (I.intSub v1 v2)):s))

      (Mul:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)  ->
         Just (c, e, (Value (IntVal (I.intMul v1 v2)):s))

      (Div:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)  ->
         Just (c, e, (Value (IntVal (I.intDiv v1 v2)):s))

      (Nand:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s) ->
         Just (c, e, (Value (IntVal (I.intNand v1 v2)):s))

      (Lt:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)   ->
         Just (c, e, (Value (BoolVal (I.intLt v1 v2)):s))

      (Eq:c, e, (Value (IntVal v2)):(Value (IntVal v1)):s)   ->
         Just (c, e, (Value (BoolVal (I.intEq v1 v2)):s))

      otherwise -> Nothing

loop :: State -> State
loop state =
    case step state of
      Just state' -> loop state'
      Nothing     -> state

eval :: S.Term -> Value
eval t = case loop (compile t, [], []) of
           (_,_,Value v:_) -> v

skipFixEnvs :: Env -> Env
skipFixEnvs e = case reverse e of
                  er -> take (skipFixWorker er 0) er

skipFixWorker :: Env -> Int -> Int
skipFixWorker [] i     = i
skipFixWorker (e:es) i = case e of
                           (CloFix _) -> i
                           otherwise  -> skipFixWorker es (i+1)
\end{code}
