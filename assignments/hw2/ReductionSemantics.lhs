\begin{code}
module ReductionSemantics where
import qualified AbstractSyntax as S
import qualified EvaluationContext as E
import qualified IntegerArithmetic as I
import Debug.Trace
\end{code}

If the term is a redex, then it should be the next thing to reduce,
  so all redexes are given the context (redex, E.Hole).
Otherwise we look for the first term in the application
  which is not a value, and we search for a redex within it.

\begin{code}
searchDeeper :: (S.Term, E.Context) -> Maybe (S.Term, E.Context)
searchDeeper (t, c) = makeEvalContext t >>= 
  \(s, e) -> return (s, E.fillWithContext c e)

makeEvalContext :: S.Term -> Maybe (S.Term, E.Context)
makeEvalContext t =
    case t of
      S.App (S.Abs x tau11 t12) t2
        | S.isValue t2 -> return (t, E.Hole)

      S.App t1 t2
        | S.isValue t1 -> searchDeeper (t2, E.AppV t1 E.Hole)
        | otherwise    -> searchDeeper (t1, E.AppT E.Hole t2)

      S.If (S.Tru) t2 t3 -> return (t, E.Hole)
      S.If (S.Fls) t2 t3 -> return (t, E.Hole)
      S.If t1 t2 t3      -> searchDeeper (t1, E.If E.Hole t2 t3)

      t@(S.Fix (S.Abs _ _ _)) -> return (t, E.Hole)
      S.Fix t                 -> searchDeeper (t, E.Fix E.Hole) 

      t@(S.Let x t1 t2)
        | S.isValue t1 -> return (t, E.Hole)
        | otherwise    -> searchDeeper (t1, E.Let x E.Hole t2)

      S.IntAdd (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntAdd t1@(S.IntConst _) t2  -> searchDeeper (t2, E.AddV t1 E.Hole)
      S.IntAdd t1 t2  -> searchDeeper (t1, E.AddT E.Hole t2)

      S.IntSub (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntSub t1@(S.IntConst _) t2  -> searchDeeper (t2, E.SubV t1 E.Hole)
      S.IntSub t1 t2  -> searchDeeper (t1, E.SubT E.Hole t2)

      S.IntMul (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntMul t1@(S.IntConst _) t2  -> searchDeeper (t2, E.MulV t1 E.Hole)
      S.IntMul t1 t2  -> searchDeeper (t1, E.MulT E.Hole t2)

      S.IntDiv (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntDiv t1@(S.IntConst _) t2  -> searchDeeper (t2, E.DivV t1 E.Hole)
      S.IntDiv t1 t2  -> searchDeeper (t1, E.DivT E.Hole t2)

      S.IntNand (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntNand t1@(S.IntConst _) t2  -> searchDeeper (t2, E.NandV t1 E.Hole)
      S.IntNand t1 t2  -> searchDeeper (t1, E.NandT E.Hole t2)

      S.IntEq (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntEq t1@(S.IntConst _) t2  -> searchDeeper (t2, E.EqV t1 E.Hole)
      S.IntEq t1 t2  -> searchDeeper (t1, E.EqT E.Hole t2)

      S.IntLt (S.IntConst _) (S.IntConst _) -> return (t, E.Hole)
      S.IntLt t1@(S.IntConst _) t2  -> searchDeeper (t2, E.LtV t1 E.Hole)
      S.IntLt t1 t2  -> searchDeeper (t1, E.LtT E.Hole t2)

      otherwise -> fail "Couldn't make an evaluation context"
\end{code}
In makeEvalContext, we need to search for a redex and the whole input term is not necessarily a redex if it is not a value. Note that a term can be redex, value, stuck, or something else. When it is something else, we search for the leftmost redex. If no redex can be found then it is a stuck term. This search can be done recursively. So, makeEvalContext should always return a subterm which is a redex so that makeContractum can reduce it. The function makeContractum reduces a redex:
\begin{flushleft}
$(\lambda x.t) v \to [x \mapsto v] t$\\
$v_1$ op $v_2 \to v_1$ $\tilde{op}$ $v_2$\\
if tru then $t_2$ else $t_3 \to t_2$\\
if fls then $t_2$ else $t_3 \to t_3$\\
let $x=v$ in $t \to [x \mapsto v]$ $t$\\
fix($\lambda x.t) \to [x \mapsto $fix($\lambda x.t)]$ $t$
\end{flushleft}

\begin{code}
makeContractum :: S.Term -> S.Term
makeContractum t =
    case t of
      S.App (S.Abs x tau11 t12) t2              -> S.subst x t2 t12
      S.If S.Tru t1 t2                          -> t1
      S.If S.Fls t1 t2                          -> t2
      t@(S.Fix (S.Abs x _ t1))                  -> S.subst x t t1
      S.Let x t1 t2                             -> S.subst x t1 t2
      S.IntAdd  (S.IntConst t1) (S.IntConst t2) -> S.IntConst (I.intAdd t1 t2)
      S.IntSub  (S.IntConst t1) (S.IntConst t2) -> S.IntConst (I.intSub t1 t2)
      S.IntMul  (S.IntConst t1) (S.IntConst t2) -> S.IntConst (I.intMul t1 t2)
      S.IntDiv  (S.IntConst t1) (S.IntConst t2) -> S.IntConst (I.intDiv t1 t2)
      S.IntNand (S.IntConst t1) (S.IntConst t2) -> S.IntConst (I.intNand t1 t2)
      S.IntEq   (S.IntConst t1) (S.IntConst t2) -> case (I.intEq t1 t2) of
                                                     True  -> S.Tru
                                                     False -> S.Fls
      S.IntLt   (S.IntConst t1) (S.IntConst t2) -> case (I.intLt t1 t2) of
                                                     True  -> S.Tru
                                                     False -> S.Fls
      otherwise                                 -> error "Error! makeContractum cannot contract!"
\end{code}

At each step, we create an evaluation context, which will either hold a redex
  in which case we reduce it, else it fails in wich case the step
  fails as well. The evaluation keeps taking steps until it fails, at which point
  the result of the previous step is returned.
  
\begin{code}
textualMachineStep :: S.Term -> Maybe S.Term
textualMachineStep t = do (t', c) <- makeEvalContext t
                          Just (E.fillWithTerm c (makeContractum t'))

textualMachineEval :: S.Term -> S.Term
textualMachineEval t =
  case textualMachineStep t of
    Just t' -> textualMachineEval t'
    Nothing -> t
\end{code}
An obvious inefficiency in this machine is the repeated partitioning of
a program into an evaluation context and a redex. Clearly, the evaluation
context of the $n + 1$st step is closely related to the one for the $n$-th step.
