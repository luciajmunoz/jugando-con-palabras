import AFD
import Anagrama
import Diccionario
import System.IO

main :: IO ()
main = do
 putStr "Hola! Elige: 1)Analizar 2)Anagrama 3)Buscar en el diccionario 4)Salir \n"
 op <- getLine
 case op of
  "1" -> analizarMenu
  "2" -> anagramaMenu
  "3" -> diccionarioMenu
  "4" -> return ()
  _ -> main

analizarMenu :: IO ()
analizarMenu = do
 putStr "Introduzca una palabra:\n"
 str <- getLine
 let res = accept fda1 str
 case res of
  True -> putStr "Esta palabra es correcta\n"
  False -> putStr "Esta palabra es incorrecta\n"
 putStr "Quieres ver si esta palabra está en el diccionario?\n"
 op <- getLine
 case op of
  "si" -> buscarPalabraDiccionario str
  _ -> return ()
 main

anagramaMenu :: IO ()
anagramaMenu = do 
 putStr "Introduzca una palabra:\n"
 str <- getLine
 let res = quitarDup $ anagrama str
 putStr $ show (res) ++ "\n" 
 putStr "Solo estos anagramas son correctos\n"
 let ok = anagramasok res 
 putStr $ show (ok) ++ "\n"
 anagramas <- anagramasDic ok
 putStr $ "Ademas solo estan en el diccionario\n"  ++ (show anagramas) ++ "\n"
 main


diccionarioMenu :: IO ()
diccionarioMenu =do
 putStr "Vamos a buscar en el diccionario, Que palabra quieres buscar? \n"
 palabra <- getLine
 buscarPalabraDiccionario palabra
 main

buscarPalabraDiccionario :: String -> IO ()
buscarPalabraDiccionario palabra = do
 res <- palabraDiccionario palabra
 case res of
  True -> putStr "Esta! \n"
  False -> putStr "No esta :( \n"
 


 
  





 






