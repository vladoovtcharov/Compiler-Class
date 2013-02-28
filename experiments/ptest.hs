{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              Rank2Types, FlexibleContexts, NoMonomorphismRestriction,
              CPP  #-}

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

ident ::  Parser String
ident = ((:) <$> pRange ('a','z') <*> pMunch (\x -> 'a' <= x && x <= 'z') `micro` 2) <* spaces

idents = pList1 ident

pKey keyw = pToken keyw `micro` 1 <* spaces

spaces :: Parser String
spaces = pMunch (`elem` " \n")

takes_second_alt =   pList ident 
               <|> (\ c t e -> ["IfThenElse"] ++  c   ++  t  ++  e) 
                   <$ pKey "if"   <*> pList_ng ident 
                   <* pKey "then" <*> pList_ng ident
                   <* pKey "else" <*> pList_ng ident  


run :: Show t =>  Parser t -> String -> IO ()
run p inp = do  let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
                putStrLn ("--  Result: " ++ show a)
                if null errors then  return ()
                               else  do putStr ("--  Correcting steps: \n")
                                        show_errors errors
                putStrLn "-- "
             where show_errors :: (Show a) => [a] -> IO ()
                   show_errors = sequence_ . (map (putStrLn . show))
