%% Literate Haskell script intended for lhs2TeX.
\documentclass[10pt]{article}
%include lhs2TeX.fmt
%format union = "\cup"
%format `union` = "\cup"
%format Hole = "\square"
%format MachineTerminate ="\varodot"
%format CEKMachineTerminate ="\varodot"
%format alpha = "\alpha"
%format gamma = "\gamma"
%format zeta = "\zeta"
%format kappa = "\kappa"
%format kappa'
%format capGamma = "\Gamma"
%format sigma = "\sigma"
%format tau = "\tau"
%format taus = "\tau s"
%format ltaus = "l\tau s"
%format tau1
%format tau1'
%format tau2
%format tau11
%format tau12
%format upsilon = "\upsilon"
%format xi = "\xi"
%format t12
%format t1
%format t1'
%format t2
%format t2'
%format t3
%format nv1

\usepackage{fullpage}
\usepackage{mathpazo}
\usepackage{graphicx}
\usepackage{color}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{soul}
\usepackage{stmaryrd}
\usepackage[parfill]{parskip}

\setlength{\parindent}{0pt}


\title{CS555 Project Exercise 3}
\author{Mahnush Movahedi, Mahdi Zamani, Vlado Ovtcharov}
\date{April 16, 2012}
\begin{document}
\maketitle


\section{DeBruijn Notation}
%include DeBruijn.lhs

\section{Natural Semantics with nameless terms}
%include NaturalSemanticsWithEnvironmentsClosuresAndDeBruijnIndices.lhs

\section{CES Machine}
%include CESMachine.lhs

%include Syntax/CPS.lhs

\section{CE3R Machine}
%include CE3Machine.lhs

\section{Main program}
\begin{code}
module Main (
    main
) where

import DeBruijn
import TypeCheck
import AbstractSyntax
import System.Environment
import qualified CESMachine as C
import qualified StructuralOperationalSemantics as S
import qualified NaturalSemanticsWithEnvironmentsClosuresAndDeBruijnIndices as N
import qualified Syntax.CPS as P
import qualified Syntax.SyntaxB as B
import qualified Syntax.SyntaxC as X
import qualified DeBruijn as D
import qualified CE3Machine as M
import qualified Semantics.SmallStepB as Small
import qualified Semantics.BigStepB as Big

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

      let deBruijnTerm = toDeBruijn term
      putStrLn ("---DeBruijn Notation:---")
      putStrLn (show (toDeBruijn term))

      putStrLn ("---Normal form (CES intermediate code):---")
      putStrLn (show (C.compile deBruijnTerm))

      putStrLn ("---Normal form (Structural semantics):---")
      putStrLn (show (S.eval term))

      putStrLn ("---Normal form (Natural semantics with DeBruijn indices):---")
      putStrLn (show (N.eval deBruijnTerm))

      putStrLn ("---Normal form (CES Machine):---")
      putStrLn (show (C.eval deBruijnTerm))


      case P.toCPSe ( B.fromSyntaxA term) of
        Just cpsTerm -> do 
                          putStrLn ("---CPS Term:---")
                          putStrLn $ show cpsTerm
                          putStrLn ("---CPS Type:---")
                          -- putStrLn $ show $ P.checkType $ X.stripTags  cpsTerm
                          putStrLn ("---CPS Eval:---")
                          putStrLn $ show $ Big.runEval $ X.stripTags cpsTerm
                          putStrLn ("---CPS DeBruijn:---")
                          putStrLn $ show $ D.toDeBruijn $ B.toSyntaxA $ X.stripTags cpsTerm


\end{code}
\end{document}
