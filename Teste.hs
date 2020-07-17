module Teste where

import Exp

type Dict = Exp String String


dic_rd :: String -> Dict -> Maybe [String]
dic_rd "" d = Nothing
dic_rd s (Term o v) = Just (aux s v ) where
                        aux "" ((Var v):(Term s d):t) = [v]
                        aux "" ((Var v):t) = [v] ++ aux "" t
                        aux "" [] = []
                        aux s ((Var v):t) = aux s t
                        aux s ((Term v l):t) = if head s == head v then aux (tail s) l else aux s t



                        
divide "" [] = i1 ()
divide "" ((Var v):(Term s d):t) = i1 [v]
divide "" ((Var v):t) = i2 ([v], "" t)
divide s ((Var v):t) = i2 (s t)
divide s ((Term v l):t) = if head s == head v then aux (tail s) l else aux s t
                      

--aux :: String -> [Dict] -> [String]




--palavra e traduções
dic_in :: String -> [String] -> Dict -> Dict
dic_in "" s d = d
dic_in "" [] d = d 
dic_in s [] d = d
dic_in s t d = if (dic_rd d) == False then aux2 s t d  else d 

aux2 :: String -> [String] -> Dict -> Dict 
aux2 s l (Term o v) | head s == o = aux2 (tail s) l 


