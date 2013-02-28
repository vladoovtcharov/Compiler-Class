Naive implementation of substitution
would be expensive. Furthermore, in the normal case that substitution
procedures a non-value expression, the machine immediately traverses
the result of the substitution again. In CEK machine, we delay the work of substitution until it
is actually needed. One way to represent delayed substitutions is to
replace expressions with a pair whose first component is an expression,
possibly with free variables, and whose second component is a substitution
that covers at least all free variables of the expression. The pair is called a closure and the
representation of the substitution is an environment.
\begin{code}
module CEKMachine where

import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

newtype Closure = Cl (S.Term, Environment)
    deriving Show

newtype Environment = Env [(S.Var, Closure)]
    deriving Show

emptyEnv :: Environment
emptyEnv = Env []

lookupEnv :: Environment -> S.Var -> Closure
lookupEnv (e@(Env [])) x  =  error ("variable " ++ x ++ " not bound in environment " ++ show e)
lookupEnv (Env ((v,c):t)) x
  | x == v     =  c
  | otherwise  =  lookupEnv (Env t) x

addEnv :: S.Var -> Closure -> Environment -> S.Term -> Environment
addEnv x c@(Cl(s,_)) (Env e) t = case t of 
                                   (S.Abs y _ t1) -> if (x == y || elem y (S.fv s)) then (Env e) else (Env ((x,c):e))
                                   otherwise      -> (Env ((x,c):e))

data Cont  =  MachineTerminate
           |  Fun     Closure Cont  -- where Closure is a value
           |  Arg     Closure Cont
           |  If      Closure Closure Cont  -- lazy
           |  Fix     Cont
           |  Let     S.Var Closure Cont
           |  Plus1   Closure Cont
           |  Plus2   Closure Cont  -- where Closure is a value
           |  Minus1  Closure Cont
           |  Minus2  Closure Cont  -- where Closure is a value
           |  Times1  Closure Cont
           |  Times2  Closure Cont  -- where Closure is a value
           |  Div1    Closure Cont
           |  Div2    Closure Cont  -- where Closure is a value
           |  Nand1   Closure Cont
           |  Nand2   Closure Cont  -- where Closure is a value
           |  Eq1     Closure Cont
           |  Eq2     Closure Cont  -- where Closure is a value
           |  Lt1     Closure Cont
           |  Lt2     Closure Cont  -- where Closure is a value
           
finalSubst t1 (Env []) = t1
finalSubst t1 (Env(e:es)) = case e of
                      (y, c) -> case c of 
                                  Cl(t2,_) -> finalSubst (S.subst y t2 t1) (Env es)

cekMachineStep :: (Closure, Cont) -> Maybe (Closure, Cont)
cekMachineStep (c, k) =
  case c of
    Cl (S.App t1 t2, e)     ->  Just (Cl(t1, e), Arg (Cl(t2, e)) k)
    Cl (S.If t1 t2 t3, e)   ->  Just (Cl(t1, e), If (Cl(t2, e)) (Cl(t3, e)) k)
    Cl (S.Fix t1, e)        ->  Just (Cl(t1, e), Fix k)
    Cl (S.Let x t1 t2, e)   ->  Just (Cl(t1, e), Let x (Cl(t2, e)) k)
    Cl (S.IntAdd t1 t2, e)  ->  Just (Cl(t1, e), Plus1 (Cl(t2, e)) k)
    Cl (S.IntSub t1 t2, e)  ->  Just (Cl(t1, e), Minus1 (Cl(t2, e)) k)
    Cl (S.IntMul t1 t2, e)  ->  Just (Cl(t1, e), Times1 (Cl(t2, e)) k)
    Cl (S.IntDiv t1 t2, e)  ->  Just (Cl(t1, e), Div1 (Cl(t2, e)) k)
    Cl (S.IntNand t1 t2, e) ->  Just (Cl(t1, e), Nand1 (Cl(t2, e)) k)
    Cl (S.IntEq t1 t2, e)   ->  Just (Cl(t1, e), Eq1 (Cl(t2, e)) k)
    Cl (S.IntLt t1 t2, e)   ->  Just (Cl(t1, e), Lt1 (Cl(t2, e)) k)
    Cl (S.Var x, e)         ->  Just (lookupEnv e x, k)

    Cl(t, e) | S.isValue t ->
      case k of
        MachineTerminate                 ->  case (t, e) of
                                               (S.Abs x _ t1, Env(_:es)) -> Just (Cl(finalSubst t e, emptyEnv), k)
                                               otherwise -> Nothing
                                               
        Arg (Cl(t1, e')) k               ->  Just (Cl(t1, e'), Fun (Cl(t, e)) k)
        Fun (Cl((S.Abs x _ t1), e')) k   ->  Just (Cl(t1, (addEnv x (Cl(t,e)) e' t1)), k)

        If (Cl(t2, e2)) (Cl(t3, e3)) k   ->  case t of
                                               S.Tru -> Just (Cl(t2, e2), k)
                                               S.Fls -> Just (Cl(t3, e3), k)

        Fix k                            ->  case t of
                                               (S.Abs x _ t1) ->
                                                 Just (Cl(t1, addEnv x (Cl(S.Fix t, e)) e t1), k)

        Let x (Cl(t2, e')) k             ->  Just (Cl(t2, addEnv x (Cl(t, e)) e' t2), k)

        Plus1 (Cl(t2, e')) k             ->  Just (Cl(t2, e'), Plus2 (Cl(t, e)) k)
        Plus2 (Cl(S.IntConst t1, e')) k  ->
            case t of (S.IntConst t2) -> Just (Cl(S.IntConst (I.intAdd t1 t2), e'), k)

        Minus1 (Cl(t2, e')) k            ->  Just (Cl(t2, e'), Minus2 (Cl(t, e)) k)
        Minus2 (Cl(S.IntConst t1, e')) k ->
            case t of (S.IntConst t2) -> Just (Cl(S.IntConst (I.intSub t1 t2), e'), k)

        Times1 (Cl(t2, e')) k            ->  Just (Cl(t2, e'), Times2 (Cl(t, e)) k)
        Times2 (Cl(S.IntConst t1, e')) k ->
            case t of (S.IntConst t2) -> Just (Cl(S.IntConst (I.intMul t1 t2), e'), k)

        Div1 (Cl(t2, e')) k              ->  Just (Cl(t2, e'), Div2 (Cl(t, e)) k)
        Div2 (Cl(S.IntConst t1, e')) k   ->
            case t of (S.IntConst t2) -> Just (Cl(S.IntConst (I.intDiv t1 t2), e'), k)

        Nand1 (Cl(t2, e')) k             ->  Just (Cl(t2, e'), Nand2 (Cl(t, e)) k)
        Nand2 (Cl(S.IntConst t1, e')) k  ->
            case t of (S.IntConst t2) -> Just (Cl(S.IntConst (I.intNand t1 t2), e'), k)

        Eq1 (Cl(t2, e')) k               ->  Just (Cl(t2, e'), Eq2 (Cl(t, e)) k)
        Eq2 (Cl(S.IntConst t1, e')) k    ->
            case t of (S.IntConst t2) -> case I.intEq t1 t2 of
                                           True  -> Just (Cl(S.Tru, e'), k)
                                           False -> Just (Cl(S.Fls, e'), k)

        Lt1 (Cl(t2, e')) k               ->  Just (Cl(t2, e'), Lt2 (Cl(t, e)) k)
        Lt2 (Cl(S.IntConst t1, e')) k    ->
            case t of (S.IntConst t2) -> case I.intLt t1 t2 of
                                           True  -> Just (Cl(S.Tru, e'), k)
                                           False -> Just (Cl(S.Fls, e'), k)

cekMachineEval' :: (Closure, Cont) -> (Closure, Cont)
cekMachineEval' (c, k) = case cekMachineStep (c, k) of
                           Just (c', k') -> cekMachineEval' (c', k')
                           Nothing       -> (c, MachineTerminate)

cekMachineEval :: S.Term -> S.Term
cekMachineEval t = case fst (cekMachineEval' (Cl(t, emptyEnv), MachineTerminate)) of
                     Cl(t', e) -> t'
\end{code}
