\begin{code}
module CE3RMachine where
import qualified CPS as C
import qualified DeBruijn as S
import qualified IntegerArithmetic as I

data Inst = Int  Integer Integer
         |  Bool Integer Bool
         |  Add
         |  Sub
         |  Mul
         |  Div
         |  Nand
         |  Eq
         |  Lt
         |  Access Integer Int
         |  Close  Integer Code
         |  App1
         |  App2
         |  If
         |  Fix
         deriving Eq

instance Show Inst where
  show  (Int j i)    =  "Int" ++ show j ++ " " ++ show i
  show  (Bool j b)   =  "Bool" ++ show j ++ " " ++ show b
  show  Add          =  "Add"
  show  Sub          =  "Sub"
  show  Mul          =  "Mul"
  show  Div          =  "Div"
  show  Nand         =  "Nand"
  show  Eq           =  "Eq"
  show  Lt           =  "Lt"
  show  (Access j i) =  "~" ++ show j ++ " " ++ show i
  show  (Close j c)  =  "Close" ++ show j ++ " " ++ show c
  show  App1         =  "App1"
  show  App2         =  "App2"
  show  If           =  "If"
  show  Fix          =  "Fix"

type Code = [Inst]
data Value = BoolVal Bool
           | IntVal Integer
           | Clo Code Env
           | CloFix Code
           | Null
    deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (IntVal i)  = show i
  show (Clo c e)   = "Clo " ++ show c ++ " " ++ show e
  show (CloFix c)  = "Clo " ++ show c
  show Null        = "Null"

type Env = [Value]
type Registers = (Value, Value, Value)
type State = (Code, Env, Registers)

compAtom :: Integer -> S.Term -> Code
compAtom j t = case t of
                 S.Tru                     ->  [Bool j True]
                 S.Fls                     ->  [Bool j False]
                 S.IntConst i              ->  [Int j i]
                 S.Var i                   ->  [Access j i]
                 S.Abs tau (S.Abs tau' t)  ->  [Close j (compile t)]
                 S.Abs tau t               ->  [Close j (compile t)]

compile :: S.Term -> Code
compile t =
    case t of
      S.App k (S.If t1 t2 t3)    ->  compAtom 1 t1 ++ compAtom 2 t2 ++
                                     compAtom 3 t3 ++ [If] ++ compAtom 1 k ++ [App1]

      S.App (S.App t1 t2) t3     ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [App2]
      S.App t1 (S.IntAdd t2 t3)  ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Add]
      S.App t1 (S.IntSub t2 t3)  ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Sub]
      S.App t1 (S.IntMul t2 t3)  ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Mul]
      S.App t1 (S.IntDiv t2 t3)  ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Div]
      S.App t1 (S.IntNand t2 t3) ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Nand]
      S.App t1 (S.IntEq t2 t3)   ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Eq]
      S.App t1 (S.IntLt t2 t3)   ->  compAtom 1 t1 ++ compAtom 2 t2 ++ compAtom 3 t3 ++ [Lt]
      S.App t1 t2                ->  compAtom 1 t1 ++ compAtom 2 t2 ++ [App1]
      t                          ->  compAtom 1 t

step :: State -> Maybe State
step state =
    case state of
      ((Int 1 i):c, e, (_,v2,v3))       ->  Just (c, e, (IntVal i,v2,v3))
      ((Int 2 i):c, e, (v1,_,v3))       ->  Just (c, e, (v1,IntVal i,v3))
      ((Int 3 i):c, e, (v1,v2,_))       ->  Just (c, e, (v1,v2,IntVal i))

      ((Bool 1 b):c, e, (_,v2,v3))      ->  Just (c, e, (BoolVal b,v2,v3))
      ((Bool 2 b):c, e, (v1,_,v3))      ->  Just (c, e, (v1,BoolVal b,v3))
      ((Bool 3 b):c, e, (v1,v2,_))      ->  Just (c, e, (v1,v2,BoolVal b))

      ((Access 1 i):c, e, (_,v2,v3))    ->  Just (c, e, (e !! i,v2,v3))
      ((Access 2 i):c, e, (v1,_,v3))    ->  Just (c, e, (v1,e !! i,v3))
      ((Access 3 i):c, e, (v1,v2,_))    ->  Just (c, e, (v1,v2,e !! i))

      ((Close 1 c'):c, e, (_,v2,v3))    ->  Just (c, e, ((Clo c' e),v2,v3))
      ((Close 2 c'):c, e, (v1,_,v3))    ->  Just (c, e, (v1,(Clo c' e),v3))
      ((Close 3 c'):c, e, (v1,v2,_))    ->  Just (c, e, (v1,v2,(Clo c' e)))

      (App1:c, e, ((Clo c' e'),v2,_))   ->  Just (c', v2:e', (Null,Null,Null))
      (App2:c, e, ((Clo c' e'),v2,v3))  ->  Just (c', v3:v2:e', (Null,Null,Null))

      (If:c, e, (BoolVal v,t2,t3))      ->
         if v == True
         then Just(c, e, (Null,t2,Null))
         else Just(c, e, (Null,t3,Null))

      (Add:c, e, ((Clo c' e'),IntVal v2,IntVal v3))   ->
            Just (c', (IntVal (I.intAdd v2 v3)):e', (Null,Null,Null))

      (Sub:c, e, ((Clo c' e'),IntVal v2,IntVal v3))   ->
            Just (c', (IntVal (I.intSub v2 v3)):e', (Null,Null,Null))

      (Mul:c, e, ((Clo c' e'),IntVal v2,IntVal v3))   ->
            Just (c', (IntVal (I.intMul v2 v3)):e', (Null,Null,Null))

      (Div:c, e, ((Clo c' e'),IntVal v2,IntVal v3))   ->
            Just (c', (IntVal (I.intDiv v2 v3)):e', (Null,Null,Null))

      (Nand:c, e, ((Clo c' e'),IntVal v2,IntVal v3))  ->
            Just (c', (IntVal (I.intNand v2 v3)):e', (Null,Null,Null))

      (Eq:c, e, ((Clo c' e'),IntVal v2,IntVal v3))    ->
            Just (c', (BoolVal (I.intEq v2 v3)):e', (Null,Null,Null))

      (Lt:c, e, ((Clo c' e'),IntVal v2,IntVal v3))    ->
            Just (c', (BoolVal (I.intLt v2 v3)):e', (Null,Null,Null))

      otherwise                                       ->  Nothing

loop :: State -> State
loop state =
    case step state of
      Just state' -> loop state'
      Nothing     -> state

eval :: S.Term -> Value
eval t = case loop (compile t, [], (Null,Null,Null)) of
           (_,_,(v,_,_)) -> v
\end{code}
