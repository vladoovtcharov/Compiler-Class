module Main (
    main
) where

import AbstractSyntax
import CommonFuncs
import Typing
import qualified NaturalSemantics as N
import qualified StructuralOperationalSemantics as S
import System.Environment

main =
    do
      args <- System.Environment.getArgs
      let [sourceFile] = args
      source <- readFile sourceFile
      putStrLn ("---Input:---")
      putStrLn (source)
      let tokens = scan source
      let term = parse tokens
      putStrLn ("---Term:---")
      putStrLn (show term)
      putStrLn ("---Type:---")
      putStrLn (show (typeCheck term))
      putStrLn ("---Normal form (Structural semantics):---")
      putStrLn (show (S.eval term))
      putStrLn ("---Normal form (Natural semantics):---")
      putStrLn (show (N.eval term))
