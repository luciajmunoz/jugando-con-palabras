module Diccionario where
import System.IO


buscar :: String -> String -> Bool
buscar [] palabra = False
buscar contents palabra = if(comparaPalabra(sacaPalabra contents []) palabra) then True else buscar (eliminaPalabra contents ) palabra

sacaPalabra :: String -> String -> String
sacaPalabra [] lf = lf
sacaPalabra (x:r) lf = if (x /= '\n') then sacaPalabra r (lf++[x]) else lf 

eliminaPalabra :: String -> String
eliminaPalabra [] = []
eliminaPalabra (x:r) = if (x /= '\n') then eliminaPalabra r else r


comparaPalabra :: String -> String -> Bool
comparaPalabra [] [] = True
comparaPalabra _ [] = False
comparaPalabra [] _ = False
comparaPalabra (x:r) (y:p) = if ( x==y ) then comparaPalabra r p else False

palabraDiccionario :: String -> IO Bool
palabraDiccionario palabra = do
 let (x:_) = palabra
 handle <- openFile ("dics/" ++ [x] ++ ".txt") ReadMode
 contents <- do { x <- hGetContents handle; return x}
 let res = buscar contents palabra
 return $ res
