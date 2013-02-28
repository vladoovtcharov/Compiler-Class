\begin{code}
module CPS where
import AbstractSyntax
import qualified TypeCheck as T

toCPS :: Type -> Term -> Term
toCPS o t = case t of

  Abs x tau t   ->  Abs k (typeToCPS o (TypeArrow tau (typeOf t)))
                         (App (Var k) (Abs x tau (toCPS o t)))
                      where k = getFresh (fv t) "k"

  App t1 t2     ->  Abs k (typeToCPS o (typeOfResult t1))
                      (App (toCPS o t1)
                         (Abs v1 (typeToCPS o (typeOf t1))
                            (App (toCPS o t2)
                               (Abs v2 (typeToCPS o (typeOf t2))
                                  (App (App (Var v1) (Var v2)) (Var k))))))

                     where k  = getFresh (fv t) "k"
                           v1 = getFresh (fv t) "v1"
                           v2 = getFresh (fv t) "v2"

  Var x         ->  Abs k o (App (Var k) t)
                     where k = getFresh [x] "k"

  IntConst x    ->  Abs "k" o (App (Var "k") t)
  Tru           ->  Abs "k" o (App (Var "k") t)
  Fls           ->  Abs "k" o (App (Var "k") t)

  Fix t         ->  case (toCPS o t) of   
                      (Abs k tau (App _ t')) ->
                        (Abs k tau (App (Fix t') (Var k)))                     

  Let s t1 t2 -> toCPS o (App (Abs s (typeOf t1) t2) t1)

  If t1 t2 t3   ->  Abs k o
                     (App (toCPS o t1)
                       (Abs v1 o
                         (App (toCPS o t2)
                           (Abs v2 o
                             (App (toCPS o t3)
                               (Abs v3 o
                                 (App (Var k) (If (Var v1) (Var v2) (Var v3)))
                               )
                             )
                           )
                         )
                       )
                     )
                     where k  = getFresh (fv t) "k"
                           v1 = getFresh (fv t) "v1"
                           v2 = getFresh (fv t) "v2"
                           v3 = getFresh (fv t) "v3"

  IntAdd a b    ->  toCPSBinOp o (IntAdd)  a b
  IntSub a b    ->  toCPSBinOp o (IntSub)  a b
  IntMul a b    ->  toCPSBinOp o (IntMul)  a b
  IntDiv a b    ->  toCPSBinOp o (IntDiv)  a b
  IntNand a b   ->  toCPSBinOp o (IntNand) a b
  IntEq a b     ->  toCPSBinOp o (IntEq)   a b
  IntLt a b     ->  toCPSBinOp o (IntLt)   a b

toCPSBinOp o op t1 t2 = Abs k (typeToCPS o (typeToCPS o TypeInt))
                            (App (toCPS o t1)
                               (Abs v1 (typeToCPS o (typeOf t1))
                                  (App (toCPS o t2)
                                     (Abs v2 (typeToCPS o (typeOf t2))
                                        (App (Var k) (op (Var v1) (Var v2)))))))
                           where k  = getFresh (fv t1 ++ fv t2) "k"
                                 v1 = getFresh (fv t1 ++ fv t2) "v1"
                                 v2 = getFresh (fv t1 ++ fv t2) "v2"

getFresh :: [String] -> String -> String
getFresh avoids x = if x `elem` avoids
                    then getFresh avoids ('a':x)
                    else x

typeOf :: Term -> Type
typeOf t = TypeBool

typeOfResult :: Term -> Type
typeOfResult t = TypeBool

typeToCPS :: Type -> Type -> Type
typeToCPS o tau =
  case tau of
    TypeInt        ->  TypeArrow (TypeArrow TypeInt  o) o
    TypeBool       ->  TypeArrow (TypeArrow TypeBool o) o
    TypeArrow a b  ->  TypeArrow (TypeArrow (typeToCPS a o) (TypeArrow (typeToCPS b o) o)) o

toCPS' :: Term -> Term
toCPS' t = toCPS (T.typeCheck t) t
\end{code}
