\begin{code}
module EvaluationContext where
import Data.List
import qualified AbstractSyntax as S
import qualified IntegerArithmetic as I

data Context = Hole
             | AppT Context S.Term
             | AppV S.Term Context
             | If Context S.Term S.Term
             | Fix Context
             | Let S.Var Context S.Term
             | AddT Context S.Term
             | AddV S.Term Context
             | SubT Context S.Term
             | SubV S.Term Context
             | MulT Context S.Term
             | MulV S.Term Context
             | DivV S.Term Context
             | DivT Context S.Term
             | NandV S.Term Context
             | NandT Context S.Term
             | EqV S.Term Context
             | EqT Context S.Term
             | LtT Context S.Term
             | LtV S.Term Context

fillWithTerm :: Context -> S.Term -> S.Term
fillWithTerm c t =
    case c of
      Hole         ->  t
      AppT c1 t2   ->  S.App (fillWithTerm c1 t) t2
      AppV t1 c2   ->  S.App t1 (fillWithTerm c2 t)
      If c1 t2 t3  ->  S.If (fillWithTerm c1 t) t2 t3
      Fix c1       ->  S.Fix (fillWithTerm c1 t)
      Let x c1 t2  ->  S.Let x (fillWithTerm c1 t) t2
      AddT c1 t1   ->  S.IntAdd (fillWithTerm c1 t) t1
      AddV t1 c1   ->  S.IntAdd t1 (fillWithTerm c1 t)
      SubT c1 t1   ->  S.IntSub (fillWithTerm c1 t) t1
      SubV t1 c1   ->  S.IntSub t1 (fillWithTerm c1 t)
      MulT c1 t1   ->  S.IntMul (fillWithTerm c1 t) t1
      MulV t1 c1   ->  S.IntMul t1 (fillWithTerm c1 t)
      DivT c1 t1   ->  S.IntDiv (fillWithTerm c1 t) t1
      DivV t1 c1   ->  S.IntDiv t1 (fillWithTerm c1 t)
      NandT c1 t1  ->  S.IntNand (fillWithTerm c1 t) t1
      NandV t1 c1  ->  S.IntNand t1 (fillWithTerm c1 t)
      EqT c1 t1    ->  S.IntEq (fillWithTerm c1 t) t1
      EqV t1 c1    ->  S.IntEq t1 (fillWithTerm c1 t)
      LtT c1 t1    ->  S.IntLt (fillWithTerm c1 t) t1
      LtV t1 c1    ->  S.IntLt t1 (fillWithTerm c1 t)

fillWithContext :: Context -> Context -> Context
fillWithContext c c' =
    case c of
      Hole         ->  c'
      AppT c1 t2   ->  AppT (fillWithContext c1 c') t2
      AppV t1 c2   ->  AppV t1 (fillWithContext c2 c')
      If c1 t2 t3  ->  If (fillWithContext c1 c') t2 t3
      Fix c1       ->  Fix (fillWithContext c1 c')
      Let x c1 t2  ->  Let x (fillWithContext c1 c') t2
      AddT c1 t2   ->  AddT (fillWithContext c1 c') t2
      AddV t1 c2   ->  AddV t1 (fillWithContext c2 c')
      SubT c1 t2   ->  SubT (fillWithContext c1 c') t2
      SubV t1 c2   ->  SubV t1 (fillWithContext c2 c')
      MulT c1 t2   ->  MulT (fillWithContext c1 c') t2
      MulV t1 c2   ->  MulV t1 (fillWithContext c2 c')
      DivT c1 t2   ->  DivT (fillWithContext c1 c') t2
      DivV t1 c2   ->  DivV t1 (fillWithContext c2 c')
      NandT c1 t2  ->  NandT (fillWithContext c1 c') t2
      NandV t1 c2  ->  NandV t1 (fillWithContext c2 c')
      EqT c1 t2    ->  EqT (fillWithContext c1 c') t2
      EqV t1 c2    ->  EqV t1 (fillWithContext c2 c')
      LtT c1 t2    ->  LtT (fillWithContext c1 c') t2
      LtV t1 c2    ->  LtV t1 (fillWithContext c2 c')
\end{code}

NOTE: We define the following function (lookupHole) as an optional method to be used in CC and SCC machines. By using this
function, the efficiency of the machines can be slighltly improved and the code becomes more readible:

\begin{code}
lookupHole :: Context -> (Context, Context)
lookupHole c =
    case c of
      t@(AppT Hole _)   ->  (Hole, t)
      t@(AppV _ Hole)   ->  (Hole, t)
      t@(If Hole _ _)   ->  (Hole, t)
      t@(Fix Hole)      ->  (Hole, t)
      t@(Let _ Hole _)  ->  (Hole, t)
      t@(AddT Hole _)   ->  (Hole, t)
      t@(AddV _ Hole)   ->  (Hole, t)
      t@(SubT Hole _)   ->  (Hole, t)
      t@(SubV _ Hole)   ->  (Hole, t)
      t@(MulT Hole _)   ->  (Hole, t)
      t@(MulV _ Hole)   ->  (Hole, t)
      t@(DivT Hole _)   ->  (Hole, t)
      t@(DivV _ Hole)   ->  (Hole, t)
      t@(NandT Hole _)  ->  (Hole, t)
      t@(NandV _ Hole)  ->  (Hole, t)
      t@(EqT Hole _)    ->  (Hole, t)
      t@(EqV _ Hole)    ->  (Hole, t)
      t@(LtT Hole _)    ->  (Hole, t)
      t@(LtV _ Hole)    ->  (Hole, t)
      AppT c1 t2        ->  (AppT c1' t2, hc)  where (c1', hc) = lookupHole c1
      AppV t1 c2        ->  (AppV t1 c2', hc)  where (c2', hc) = lookupHole c2
      If c1 t1 t2       ->  (If c1' t1 t2, hc) where (c1', hc) = lookupHole c1
      Fix c1            ->  (Fix c1', hc)      where (c1', hc) = lookupHole c1
      Let x c1 t2       ->  (Let x c1' t2, hc) where (c1', hc) = lookupHole c1
      AddT c1 t2        ->  (AddT c1' t2, hc)  where (c1', hc) = lookupHole c1
      AddV t1 c2        ->  (AddV t1 c2', hc)  where (c2', hc) = lookupHole c2
      SubT c1 t2        ->  (SubT c1' t2, hc)  where (c1', hc) = lookupHole c1
      SubV t1 c2        ->  (SubV t1 c2', hc)  where (c2', hc) = lookupHole c2
      MulT c1 t2        ->  (MulT c1' t2, hc)  where (c1', hc) = lookupHole c1
      MulV t1 c2        ->  (MulV t1 c2', hc)  where (c2', hc) = lookupHole c2
      DivT c1 t2        ->  (DivT c1' t2, hc)  where (c1', hc) = lookupHole c1
      DivV t1 c2        ->  (DivV t1 c2', hc)  where (c2', hc) = lookupHole c2
      NandT c1 t2       ->  (NandT c1' t2, hc) where (c1', hc) = lookupHole c1
      NandV t1 c2       ->  (NandV t1 c2', hc) where (c2', hc) = lookupHole c2
      EqT c1 t2         ->  (EqT c1' t2, hc)   where (c1', hc) = lookupHole c1
      EqV t1 c2         ->  (EqV t1 c2', hc)   where (c2', hc) = lookupHole c2
      LtT c1 t2         ->  (LtT c1' t2, hc)   where (c1', hc) = lookupHole c1
      LtV t1 c2         ->  (LtV t1 c2', hc)   where (c2', hc) = lookupHole c2
      otherwise         ->  error "Evaluation error."
\end{code}

As a replacement for lookupHole, we can call fillWithTerm function to fill the hole with the value and then, call makeEvalContext to find the next redex. This is done instead of finding the hole in the context, which is done in lookupHole. Thus, in CC and SCC machines we can use the following code instead of calling lookupHole:

<ccMachineStep (t, c) =
<  case t of
<  ...
<    t | S.isValue t ->
<      case c of
<        E.Hole    -> Nothing
<        otherwise -> Just (makeEvalContext (E.fillWithTerm c t))
<  ...

In the latter code, fillWithTerm looks for the hole in c and then makeEvalContext looks for the redex. So, we do one unnecessary search, which is avoided when using lookupHole.