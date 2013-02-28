%% Literate Haskell script intended for lhs2TeX.

\documentclass[10pt]{article}
%include lhs2Tex.fmt


\title{CS555 Project Exercise 3}
\author{Mahnush Movahedi, Mahdi Zamani, Vlado Ovtcharov}
\date{April 16, 2012}
\begin{document}
\maketitle
\fontsize{3.5mm}{5.3mm}\selectfont
\thispagestyle{empty}
In this exercise, we implemented the nameless representation (De Bruijn notation), natural semantics with nameless representation, the CES machine, the Continuation-Passing-Style (CPS) transformation, and the CE3R machine. Our solution for adding types to the CPS is based on [1]. We have also implemented the solution proposed in [2] as an alternative CPS tranformation method.

\section{De Bruijn notation}
%include DeBruijn.lhs

\section{Natural semantics with nameless terms}
%include NaturalSemanticsWithEnvironmentsClosuresAndDeBruijnIndices.lhs

\section{CES compiler and virtual machine}
%include CESMachine.lhs

\section{CPS transformation}
%include CPS.lhs
\subsection{Alternative solution}
%include Syntax/CPS.lhs

\section{CE3R compiler and virtual machine}
%include CE3RMachine.lhs

\section{Main program}
\begin{code}
module Main (
    main
) where

import TypeCheck
import AbstractSyntax
import System.Environment
import qualified DeBruijn as D
import qualified CESMachine as C
import qualified StructuralOperationalSemantics as S
import qualified NaturalSemanticsWithEnvironmentsClosuresAndDeBruijnIndices as N
import qualified Syntax.CPS as SP
import qualified CPS as P
import qualified Syntax.SyntaxB as B
import qualified Syntax.SyntaxC as X
import qualified CE3RMachine as R

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

      let tau = typeCheck term
      putStrLn ("---Type:---")
      putStrLn (show tau)

      let deBruijnTerm = D.toDeBruijn term
      putStrLn ("---DeBruijn Notation:---")
      putStrLn (show (deBruijnTerm))

      putStrLn ("---Normal form (CES intermediate code):---")
      putStrLn (show (C.compile deBruijnTerm))

      putStrLn ("---Normal form (Structural semantics):---")
      putStrLn (show (S.eval term))

      putStrLn ("---Normal form (Natural semantics with DeBruijn indices):---")
      putStrLn (show (N.eval deBruijnTerm))

      putStrLn ("---Normal form (CES machine):---")
      putStrLn (show (C.eval deBruijnTerm))

      let cpsTerm1 = P.toCPS' term
      let cpsDeBruijnTerm1 = D.toDeBruijn (App cpsTerm1 (Abs "a" tau (Var "a")))

      putStrLn ("---Normal form (CES machine on nameless CPS1):---")
      putStrLn (show (C.eval cpsDeBruijnTerm1))

      -- putStrLn ("---Normal form (CE3R machine on CPS1):---")
      -- putStrLn (show (R.eval cpsDeBruijnTerm1))

      putStrLn ("---CPS2 Type:---")
      putStrLn (show (SP.checkType (X.stripTags cpsTerm2)))
      p
      let cpsTerm2 = case SP.toCPSe (B.fromSyntaxA term) of
                       Just t    -> t
                       otherwise -> error "CPS error"
                           
      
      let cpsDeBruijnTerm2 = D.toDeBruijn (B.toSyntaxA (X.stripTags cpsTerm2))

      putStrLn ("---Normal form (CE3R machine on CPS2):---")
      putStrLn (show (R.eval cpsDeBruijnTerm2))
\end{code}

\section{Appendix: Test results}
\subsection{Test 1}
\begin{verbatim}
\end{verbatim}

\begin{thebibliography}{1}
\bibitem{griffin} Timothy G. Griffin, {\em A Formulae-as-Types Notion of Control}, 1990: In Conference Record of the Seventeenth Annual ACM Symposium on Principles of Programming Languages, ACM Press.

\bibitem{danvy} Olivier Danvy and Andrzej Filinski, {\em Representing control: a study of the CPS transformation}, 1992.
\end{thebibliography}

\end{document}
