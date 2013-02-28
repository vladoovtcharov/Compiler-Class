\begin{code}
module LatexDefs where
class (Show x) => LatexShow x where
  latexShow :: x -> String
  latexShow = show
\end{code}
