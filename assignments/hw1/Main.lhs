\documentclass[11pt]{article}
%% Literate Haskell script intended for lhs2TeX.

%include polycode.fmt
%format gamma = "\gamma"
%format capGamma = "\Gamma"
%format tau = "\tau"
%format tau1 = "\tau_{1}"
%format tau2 = "\tau_{2}"
%format tau11 = "\tau_{11}"
%format tau12 = "\tau_{12}"
%format t12 = "t_{12}"
%format t1 = "t_{1}"
%format t1' = "t_{1}^{\prime}"
%format t2 = "t_{2}"
%format t2' = "t_{2}^{\prime}"
%format t3 = "t_{3}"

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

\title{CS555 Project Exercise 1}
\author{Mahnush Movahedi, Mahdi Zamani, Vlado Ovtcharov}
\date{February 27, 2011}
\begin{document}
\maketitle
\thispagestyle{empty}
In this exercise, we implemented a small core lambda language, consisting of the lambda calculus with booleans and integers. We also formally stated the rules that give the structural operationl semantics and natural semantics of the core lambda language. Finally, we tested the program with several test cases, which are all listed at the end of this write-up along with the results.

\section{Concrete syntax}
\begin{verbatim}
Type --> arrow lpar Type comma Type rpar
       | Bool_keyword
       | Int_keyword

Term --> identifier
       | abs_keyword lpar identifier colon Type fullstop Term rpar
       | app_keyword lpar Term comma Term rpar
       | true_keyword
       | false_keyword
       | if_keyword Term then_keyword Term else_keyword Term fi_keyword
       | intliteral
       | plus lpat Term comma Term rpar
       | minus lpar Term comma Term rpar
       | mul lpar Term comma Term rpar
       | div lpar Term comma Term rpar
       | nand lpar Term comma Term rpar
       | equal lpar Term comma Term rpar
       | lt  lpar Term comma Term rpar
       | lpar Term rpar
\end{verbatim}

\newpage
%include AbstractSyntax.lhs

\section{Type-checking}
%include Typing.lhs

\section{Evaluation}
\subsection{Integer arithmetics}
%include IntegerArithmetic.lhs
\subsection{Structural Operational Semantics}
\subsubsection{Rules}
\input{Include/StructuralSemantics.tex}
\subsubsection{Source codes}
%include StructuralOperationalSemantics.lhs

\subsection{Natural Semantics}
\subsubsection{Rules}
\input{Include/NaturalSemantics.tex}

\subsubsection{Source codes}
%include NaturalSemantics.lhs

\newpage

\section{Main program}
\begin{code}
module Main (
    main
) where

import Typing
import AbstractSyntax
import System.Environment
import qualified NaturalSemantics as N
import qualified StructuralOperationalSemantics as S

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
\end{code}

\section{Test results}
\subsection{Test 1}
\begin{verbatim}
---Input:---
app (abs (x: Int . 1234), 10)
---Term:---
app(abs(x:Int.1234),10)
---Type:---
Int
---Normal form (Structural semantics):---
1234
---Normal form (Natural semantics):---
1234
\end{verbatim}

\subsection{Test 2}
\begin{verbatim}
---Input:---
if true then true else false fi
---Term:---
if true then true else false fi
---Type:---
Bool
---Normal form (Structural semantics):---
true
---Normal form (Natural semantics):---
true
\end{verbatim}

\subsection{Test 3}
\begin{verbatim}
---Input:---
if =(0,0) then 8 else 9 fi
---Term:---
if 0=0 then 8 else 9 fi
---Type:---
Int
---Normal form (Structural semantics):---
8
---Normal form (Natural semantics):---
8
\end{verbatim}

\subsection{Test 4}
\begin{verbatim}
---Input:---
/(4294967295,76)
---Term:---
4294967295/76
---Type:---
Int
---Normal form (Structural semantics):---
56512727
---Normal form (Natural semantics):---
56512727
\end{verbatim}

\subsection{Test 5}
\begin{verbatim}
---Input:---
app( abs(y: Int . if =(^(-(*(+(5,7),3),/(124,62)),y),2) then 1 else app( abs(x:
Int . -(*(x,3),/(20,4))), 5) fi), 29)
---Term:---
app(abs(y:Int.if 5+7*3-124/62|"y"=2 then 1 else app(abs(x:Int."x"*3-20/4),5) fi)
,29)
---Type:---
Int
---Normal form (Structural semantics):---
10
---Normal form (Natural semantics):---
10
\end{verbatim}

\subsection{Test 6}
\begin{verbatim}
---Input:---
app(
   abs(x:->(Int,Int).
      abs(y:->(->(Int,Int),Int).
         app(y,x)
      )
   ),
   abs(y:Int.y)
)
---Term:---
app(abs(x:->(Int,Int).abs(y:->(->(Int,Int),Int).app("y","x"))),abs(y:Int."y"))
---Type:---
->(->(->(Int,Int),Int),Int)
---Normal form (Structural semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
---Normal form (Natural semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
\end{verbatim}

\subsection{Test 7}
\begin{verbatim}
---Input:---
app (
    abs (x: Int.
        if <(/(12,3),*(2,6)) then
            app( abs(x: Int . if <(x,10) then *(x,3) else *(x,4) fi), *(x,15))
        else 7
        fi
    ), 2
)
---Term:---
app(abs(x:Int.if 12/3<2*6 then app(abs(x:Int.if "x"<10 then "x"*3 else "x"*4 fi)
,"x"*15) else 7 fi),2)
---Type:---
Int
---Normal form (Structural semantics):---
120
---Normal form (Natural semantics):---
120
\end{verbatim}
\end{document}


