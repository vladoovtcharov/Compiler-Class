\begin{code}
module CE3Machine where
import qualified DeBruijn as S
import qualified IntegerArithmetic as I

data Register = R_1 | R_2 | R_3
              deriving (Eq, Show)

data Inst = Int Register Integer
         |  Bool Register Bool
         |  Add
         |  Sub
         |  Mul
         |  Div
         |  Nand
         |  Eq
         |  Lt
         |  Access Register Int
         |  Close Register Code
         |  Let
         |  EndLet
         |  Apply
         |  Return
         |  If
         |  Fix
         |  TailApply1
         |  TailApply2
         deriving (Show, Eq)

type Code = [Inst]
data Value = BoolVal Bool | IntVal Integer | Clo Code Env | CloFix Code
    deriving Eq

instance Show Value where
  show (BoolVal b) = show b
  show (IntVal i)  = show i
  show (Clo t e)   = "Clo " ++ show t ++ " " ++ show e
  show (CloFix t)  = "Clo " ++ show t

type Env = [Value]
data Slot = Value Value | Code Code | Env Env | Inst Inst | Empty
    deriving Show

type Stack = [Slot]

type Registers = (Slot, Slot, Slot)

type State = (Code, Env, Registers)


compile :: S.Term -> Code
compile t = case t of
  S.Var _ -> aA R_1 t
  S.Tru   -> aA R_1 t
  S.Fls   -> aA R_1 t
  S.IntConst _ -> aA R_1 t
  S.Abs _ _ -> aA R_1 t
  S.App (S.App t1 t2) t3 -> (aA R_1 t1) ++ (aA R_2 t2) ++ (aA R_3 t3) ++ [TailApply2]
  S.App t1 t2     -> (aA R_1 t1) ++ (aA R_2 t2) ++ [TailApply1]

aA :: Register -> S.Term -> Code
aA r t = case t of
  S.Var x                 -> [Access r x]
  S.Tru                   -> [Bool r True]
  S.Fls                   -> [Bool r False]
  S.IntConst x            -> [Int r x]
  S.Abs _ (S.Abs _ t)     -> [Close r (compile t)]
  S.Abs _ t               -> [Close r (compile t)]
  otherwise -> error$ "error " ++ show t
 
repAt :: Register -> Slot -> Registers -> Registers
repAt R_1 i (_, v2, v3) = (i, v2, v3)
repAt R_2 i (v1, _, v3) = (v1, i, v3)
repAt R_3 i (v1, v2, _) = (v1, v2, i)
 
step :: State -> Maybe State
step state =
    case state of
      ((Int r v):c, e, rs)      ->
        Just (c, e,  repAt r (Value (IntVal v)) rs )
      ((Bool r v):c, e, rs)      ->
        Just (c, e,  repAt r (Value (BoolVal v)) rs)

      ((Access r n):c, e, rs)    ->  
         case e !! n of
           CloFix t  ->  Just (t++c, e, rs)
           v         ->  Just (c, e, repAt r (Value v) rs)


      ((Close r c'):c, e, rs)    ->
         Just (c, e, repAt r (Value (Clo c' e)) rs)

      (TailApply1:c, e, (Value (Clo c' e'), Value v1, _)) ->
        Just (c', v1:e', (Empty, Empty, Empty))

      (TailApply2:c, e, (Value (Clo c' e'), Value v1, Value v2)) ->
        Just (c', v2:v1:e', (Empty, Empty, Empty))

      otherwise -> Nothing


loop :: State -> State
loop state =
    case step state of
      Just state' -> loop state'
      Nothing     -> state

eval :: S.Term -> Value
eval t = case loop (compile t, [], (Empty, Empty, Empty)) of
           (_,_, (Value v,_,_)) -> v
\end{code}
