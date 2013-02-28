module Main (
    main
) where

-- import TypeCheck
import AbstractSyntax
import System.Environment
import TypeCheck
import qualified NaturalSemantics as N
import qualified StructuralOperationalSemantics as S
-- import qualified ReductionSemantics as R
main =
    do
      args <- System.Environment.getArgs
      let [sourceFile] = args
      source <- readFile sourceFile
      let term = case readTerm source of
                   Right t -> t
                   Left err -> error $ show err
      putStrLn ("---Term:---")
      putStrLn (show term)
      putStrLn ("---Type:---")
      putStrLn (show (typeCheck term))
      -- putStrLn ("---Normal Form (Reduction Semantic):---")
      -- putStrLn (show (R.textualMachineEval term))
      putStrLn ("---Normal form (Structural semantics):---")
      normStruc <- S.eval term
      putStrLn (show normStruc)
      putStrLn ("---Normal form (Natural semantics):---")
      putStrLn (show (N.eval term))


