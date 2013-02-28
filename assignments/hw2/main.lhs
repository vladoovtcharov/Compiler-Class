%% Literate Haskell script intended for lhs2TeX.

\documentclass[10pt]{article}
%include polycode.fmt

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
\usepackage[centertags]{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{amsthm}
\usepackage{stmaryrd}

\title{CS555 Project Exercise 2}
\author{Mahnush Movahedi, Mahdi Zamani, Vlado Ovtcharov}
\date{March 21, 2012}
\begin{document}
\maketitle
\thispagestyle{empty}
In this exercise, we added let-bindings and general recursion using fix to the language of Exercise 1.1. Also, we implemented the standard reduction, CC, SCC, CK, and CEK machines as described in Chapter 5 and 6 of the PLT Redex book.
 
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
       | fix_keyword lpar Term rpar
       | let_keyword identifier equal Term in_keyword Term end_keyword
       | add lpar Term comma Term rpar
       | sub lpar Term comma Term rpar
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
\subsection{Fix and Let rules}
\input{Include/FixLetTyping.tex}
\subsection{Source codes}
%include TypeCheck.lhs

\section{Evaluation}
\subsection{Structural operational semantics}
\subsubsection{Fix and Let Rules}
\input{Include/FixLetStructuralSemantics.tex}
\subsubsection{Source codes}
%include StructuralOperationalSemantics.lhs

\subsection{Natural semantics}
\subsubsection{Fix and Let rules}
\input{Include/FixLetNaturalSemantics.tex}
\subsubsection{Source codes}
%include NaturalSemantics.lhs

\subsection{Auxiliary functions}
%include EvaluationContext.lhs

\subsection{Textual machine (Reduction semantics)}
%include ReductionSemantics.lhs

\subsection{CC machine}
%include CCMachine.lhs

\subsection{SCC machine}
%include SCCMachine.lhs

\subsection{CK machine}
%include CKMachine.lhs

\subsection{CEK machine}
%include CEKMachine.lhs

\section{Main program}
\begin{code}
module Main (
    main
) where

import TypeCheck
import AbstractSyntax
import System.Environment
import qualified NaturalSemantics as N
import qualified StructuralOperationalSemantics as S
import qualified ReductionSemantics as R
import qualified CCMachine as CC
import qualified SCCMachine as SCC
import qualified CKMachine as CK
import qualified CEKMachine as CEK

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
      putStrLn ("---Normal Form (Reduction semantics):---")
      putStrLn (show (R.textualMachineEval term))
      putStrLn ("---Normal Form (CC machine semantics):---")
      putStrLn (show (CC.ccMachineEval term))
      putStrLn ("---Normal Form (SCC machine semantics):---")
      putStrLn (show (SCC.sccMachineEval term))
      putStrLn ("---Normal Form (CK machine semantics):---")
      putStrLn (show (CK.ckMachineEval term))
      putStrLn ("---Normal Form (CEK machine semantics):---")
      let cekResult = CEK.cekMachineEval term
      case cekResult of
        (Abs _ _ _) -> putStrLn("Function: " ++ (show cekResult))
        otherwise   -> putStrLn (show (CEK.cekMachineEval term))
\end{code}

\section{Test results}
\subsection{Test 1}
\begin{verbatim}
---Input:---
let
  iseven =
    let
      mod = abs (m:Int. abs (n:Int. -(m,*(n,/(m,n)))))
    in
      abs (k:Int. =(0, app(app(mod,k),2)))
    end
in
  app (iseven, 7)
end
---Term:---
let iseven = let mod = abs(m:Int.abs(n:Int."m"-"n"*"m"/"n")) in abs(k:Int.0=app(
app("mod","k"),2)) in app("iseven",7)
---Type:---
Bool
---Normal form (Structural semantics):---
false
---Normal form (Natural semantics):---
false
---Normal Form (Reduction semantics):---
false
---Normal Form (CC machine semantics):---
false
---Normal Form (SCC machine semantics):---
false
---Normal Form (CK machine semantics):---
false
---Normal Form (CEK machine semantics):---
false
\end{verbatim}

\subsection{Test 2}
\begin{verbatim}
---Input:---
app (fix (abs (ie:->(Int,Bool). abs (x:Int. if =(0,x) then true else
if =(0, -(x,1)) then false else app (ie, -(x,2)) fi fi))), 7)
---Term:---
app(fix(abs(ie:->(Int,Bool).abs(x:Int.if 0="x" then true else if 0="x"-1 then fa
lse else app("ie","x"-2) fi fi))),7)
---Type:---
Bool
---Normal form (Structural semantics):---
false
---Normal form (Natural semantics):---
false
---Normal Form (Reduction semantics):---
false
---Normal Form (CC machine semantics):---
false
---Normal Form (SCC machine semantics):---
false
---Normal Form (CK machine semantics):---
false
---Normal Form (CEK machine semantics):---
false
\end{verbatim}

\subsection{Test 3}
\begin{verbatim}
---Input:---
app (app (fix (abs (e:->(Int,->(Int,Int)). abs (x:Int. abs (y: Int.
if =(0,y) then 1 else *(x,app(app(e,x),-(y,1))) fi)))), 2), 3)
---Term:---
app(app(fix(abs(e:->(Int,->(Int,Int)).abs(x:Int.abs(y:Int.if 0="y" then 1 else "
x"*app(app("e","x"),"y"-1) fi)))),2),3)
---Type:---
Int
---Normal form (Structural semantics):---
8
---Normal form (Natural semantics):---
8
---Normal Form (Reduction semantics):---
8
---Normal Form (CC machine semantics):---
8
---Normal Form (SCC machine semantics):---
8
---Normal Form (CK machine semantics):---
8
---Normal Form (CEK machine semantics):---
8
\end{verbatim}

\subsection{Test 4}
\begin{verbatim}
---Input:---
let
   iseven = fix (abs (ie:->(Int,Bool). abs (x:Int.
              if =(0,x) then true else
                if =(1,x) then false else
                  app (ie, -(x,2)) fi fi)))
in
  let
    collatz = fix (abs (collatz:->(Int,Int). abs (x: Int.
                if app (iseven, x) then app (collatz, /(x,2)) else
                  if =(x,1) then 1 else
                    app (collatz, +(*(3,x),1)) fi fi)))
  in
    app (collatz, 1000)
  end
end
---Term:---
let iseven = fix(abs(ie:->(Int,Bool).abs(x:Int.if 0="x" then true else if 1="x"
then false else app("ie","x"-2) fi fi))) in let collatz = fix(abs(collatz:->(Int
,Int).abs(x:Int.if app("iseven","x") then app("collatz","x"/2) else if "x"=1 the
n 1 else app("collatz",3*"x"+1) fi fi))) in app("collatz",1000)
---Type:---
Int
---Normal form (Structural semantics):---
1
---Normal form (Natural semantics):---
1
---Normal Form (Reduction semantics):---
1
---Normal Form (CC machine semantics):---
1
---Normal Form (SCC machine semantics):---
1
---Normal Form (CK machine semantics):---
1
---Normal Form (CEK machine semantics):---
1
\end{verbatim}

\subsection{Test 5}
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
---Normal Form (Reduction semantics):---
120
---Normal Form (CC machine semantics):---
120
---Normal Form (SCC machine semantics):---
120
---Normal Form (CK machine semantics):---
120
---Normal Form (CEK machine semantics):---
120
\end{verbatim}

\subsection{Test 6}
\begin{verbatim}
---Input:---
+(7,if <(5,3) then 4 else 6 fi)
---Term:---
7+if 5<3 then 4 else 6 fi
---Type:---
Int
---Normal form (Structural semantics):---
13
---Normal form (Natural semantics):---
13
---Normal Form (Reduction semantics):---
13
---Normal Form (CC machine semantics):---
13
---Normal Form (SCC machine semantics):---
13
---Normal Form (CK machine semantics):---
13
---Normal Form (CEK machine semantics):---
13
\end{verbatim}

\subsection{Test 7}
\begin{verbatim}
---Input:---
app(
    fix (
        abs (f:->(Int,Int).
            abs (n:Int.
                if =(n,0) then
                    1
                else
                    *(n, app (f,-(n,1)))
                fi
            )
        )
    ), 6
)
---Term:---
app(fix(abs(f:->(Int,Int).abs(n:Int.if "n"=0 then 1 else "n"*app("f","n"-1) fi))
),6)
---Type:---
Int
---Normal form (Structural semantics):---
720
---Normal form (Natural semantics):---
720
---Normal Form (Reduction semantics):---
720
---Normal Form (CC machine semantics):---
720
---Normal Form (SCC machine semantics):---
720
---Normal Form (CK machine semantics):---
720
---Normal Form (CEK machine semantics):---
720
\end{verbatim}

\subsection{Test 8}
\begin{verbatim}
---Input:---
+(+(+(5,3),+(9,10)),+(+(15,13),14))
---Term:---
5+3+9+10+15+13+14
---Type:---
Int
---Normal form (Structural semantics):---
69
---Normal form (Natural semantics):---
69
---Normal Form (Reduction semantics):---
69
---Normal Form (CC machine semantics):---
69
---Normal Form (SCC machine semantics):---
69
---Normal Form (CK machine semantics):---
69
---Normal Form (CEK machine semantics):---
69
\end{verbatim}

\subsection{Test 9}
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
---Normal Form (Reduction semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
---Normal Form (CC machine semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
---Normal Form (SCC machine semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
---Normal Form (CK machine semantics):---
abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
---Normal Form (CEK machine semantics):---
Function: abs(y:->(->(Int,Int),Int).app("y",abs(y:Int."y")))
\end{verbatim}

\subsection{Test 10}
\begin{verbatim}
---Input:---
app(
   abs(x:Int.
      +(
        app(
            abs(z:Int.
               +(
                 app(
                    abs(x:Int.+(x,z)),
                    5
                 ),
                 app(
                    abs(y:Int.+(y,z)),
                    6
                 )
               )            
            ), 7
        ), x
      )
   ),
   8
)
---Term:---
app(abs(x:Int.app(abs(z:Int.app(abs(x:Int."x"+"z"),5)+app(abs(y:Int."y"+"z"),6)),7)+"x"),8)
---Type:---
Int
---Normal form (Structural semantics):---
33
---Normal form (Natural semantics):---
33
---Normal Form (Reduction semantics):---
33
---Normal Form (CC machine semantics):---
33
---Normal Form (SCC machine semantics):---
33
---Normal Form (CK machine semantics):---
33
---Normal Form (CEK machine semantics):---
33
\end{verbatim}

\end{document}

