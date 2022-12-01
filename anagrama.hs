module Anagrama where 
import AFD
import Data.List
import Diccionario

--realiza una lista de todas las permutaciones de del string
anagrama :: [Char] -> [[Char]]
anagrama [] = [[]]
anagrama (x:xs) = concat [intercala x ys | ys <- anagrama xs]

--Con esta funcion vamos intercalando letras 
intercala :: Char -> [Char] -> [[Char]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- con esta funcion quitamos los anagramas duplicados
quitarDup :: [[Char]] -> [[Char]]
quitarDup [] = []
quitarDup (p:r) = if (foldr (||) False $ map (== p) r) then quitarDup r else p: quitarDup r

--funcion que comprueba cuales de los anagramas generados son correctos y los inserta en una lista final
anagramasok :: [[Char]] -> [[Char]]
anagramasok []  = []
anagramasok (p:r)  = if (accept fda1 p) then p:anagramasok r else anagramasok r 

anagramasDic :: [[Char]] -> IO [[Char]]
anagramasDic []  = return []
anagramasDic (p:r)  = do
 res <- palabraDiccionario p 
 siguiente <- anagramasDic r
 case res of
  True -> return $ p:siguiente
  False -> return siguiente

