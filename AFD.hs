module AFD where 
-- Tipo de dato algebraico para simular los estados de un autómata
 data State = Q Int deriving (Show, Eq)

 -- Sinónimos para símbolos, alfabeto y la función de transición
 type Symbol = Char
 type Alphabet = [Char] 
 type Delta = State -> Symbol -> State 
 -- Tipo de dato algebraico Automata, implementación de un autómata finito determinista 
 data Automata = FDA {q::[State], s::Alphabet, d::Delta, q0::State, f::[State]}
 -- q : Conjunto de Estados
 -- s : Alfabeto de entrada
 -- d : Función de transición
 -- q0 : Estado inicial
 -- f : Conjunto de estados finales

 -- función de transición extendida, es necesario cargar con la función de transición >:(
 deltaStar :: State -> String -> Delta -> State
 deltaStar q "" _ = q
 deltaStar q (a:w) d = deltaStar (d q a) w d

 -- función que decide si una cadena es aceptada o no por un autómata. 
 accept :: Automata -> String -> Bool
 accept fda w = deltaStar (q0 fda) w (d fda) `elem` (f fda)

 -- definimos la función de transición 

 delta1 :: Delta
 delta1 (Q 0) 'b' = (Q 1)
 delta1 (Q 0) 'c' = (Q 4)
 delta1 (Q 0) 'd' = (Q 1)
 delta1 (Q 0) 'f' = (Q 1)
 delta1 (Q 0) 'g' = (Q 1)
 delta1 (Q 0) 'h' = (Q 2)
 delta1 (Q 0) 'j' = (Q 1)
 delta1 (Q 0) 'k' = (Q 1)
 delta1 (Q 0) 'l' = (Q 1)
 delta1 (Q 0) 'm' = (Q 1)
 delta1 (Q 0) 'n' = (Q 1)
 delta1 (Q 0) 'p' = (Q 1)
 delta1 (Q 0) 'q' = (Q 1)
 delta1 (Q 0) 'r' = (Q 1)
 delta1 (Q 0) 's' = (Q 1)
 delta1 (Q 0) 't' = (Q 1)
 delta1 (Q 0) 'v' = (Q 1)
 delta1 (Q 0) 'w' = (Q 1)
 delta1 (Q 0) 'x' = (Q 1)
 delta1 (Q 0) 'y' = (Q 1)
 delta1 (Q 0) 'z' = (Q 1)
 delta1 (Q 0) 'a' = (Q 3)
 delta1 (Q 0) 'e' = (Q 3)
 delta1 (Q 0) 'i' = (Q 3)
 delta1 (Q 0) 'o' = (Q 3)
 delta1 (Q 0) 'u' = (Q 3)

 delta1 (Q 1) 'l' = (Q 2)
 delta1 (Q 1) 'r' = (Q 2)
 delta1 (Q 1) 'a' = (Q 3)
 delta1 (Q 1) 'e' = (Q 3)
 delta1 (Q 1) 'i' = (Q 3)
 delta1 (Q 1) 'o' = (Q 3)
 delta1 (Q 1) 'u' = (Q 3)

 delta1 (Q 2) 'a' = (Q 3)
 delta1 (Q 2) 'e' = (Q 3)
 delta1 (Q 2) 'i' = (Q 3)
 delta1 (Q 2) 'o' = (Q 3)
 delta1 (Q 2) 'u' = (Q 3)

 delta1 (Q 3) 'b' = (Q 1)
 delta1 (Q 3) 'c' = (Q 4)
 delta1 (Q 3) 'd' = (Q 1)
 delta1 (Q 3) 'f' = (Q 1)
 delta1 (Q 3) 'g' = (Q 1)
 delta1 (Q 3) 'h' = (Q 1)
 delta1 (Q 3) 'j' = (Q 1)
 delta1 (Q 3) 'k' = (Q 1)
 delta1 (Q 3) 'm' = (Q 1)
 delta1 (Q 3) 'p' = (Q 1)
 delta1 (Q 3) 'q' = (Q 1)
 delta1 (Q 3) 't' = (Q 1)
 delta1 (Q 3) 'v' = (Q 1)
 delta1 (Q 3) 'w' = (Q 1)
 delta1 (Q 3) 'x' = (Q 1)
 delta1 (Q 3) 'y' = (Q 1)
 delta1 (Q 3) 'z' = (Q 1)
 delta1 (Q 3) 'a' = (Q 3)
 delta1 (Q 3) 'e' = (Q 3)
 delta1 (Q 3) 'i' = (Q 3)
 delta1 (Q 3) 'o' = (Q 3)
 delta1 (Q 3) 'u' = (Q 3)
 delta1 (Q 3) 'l' = (Q 6)
 delta1 (Q 3) 'n' = (Q 6)
 delta1 (Q 3) 'r' = (Q 6)
 delta1 (Q 3) 's' = (Q 6)


 delta1 (Q 4) 'h' = (Q 5)
 delta1 (Q 4) 'a' = (Q 3)
 delta1 (Q 4) 'e' = (Q 3)
 delta1 (Q 4) 'i' = (Q 3)
 delta1 (Q 4) 'o' = (Q 3)
 delta1 (Q 4) 'u' = (Q 3)

 delta1 (Q 5) 'a' = (Q 3)
 delta1 (Q 5) 'e' = (Q 3)
 delta1 (Q 5) 'i' = (Q 3)
 delta1 (Q 5) 'o' = (Q 3)
 delta1 (Q 5) 'u' = (Q 3)
 delta1 (Q 5) 'l' = (Q 2)
 delta1 (Q 5) 'r' = (Q 2)

 delta1 (Q 6) 'b' = (Q 1)
 delta1 (Q 6) 'c' = (Q 1)
 delta1 (Q 6) 'd' = (Q 1)
 delta1 (Q 6) 'f' = (Q 1)
 delta1 (Q 6) 'g' = (Q 1)
 delta1 (Q 6) 'h' = (Q 1)
 delta1 (Q 6) 'j' = (Q 1)
 delta1 (Q 6) 'k' = (Q 1)
 delta1 (Q 6) 'm' = (Q 1)
 delta1 (Q 6) 'p' = (Q 1)
 delta1 (Q 6) 'q' = (Q 1)
 delta1 (Q 6) 't' = (Q 1)
 delta1 (Q 6) 'v' = (Q 1)
 delta1 (Q 6) 'w' = (Q 1)
 delta1 (Q 6) 'x' = (Q 1)
 delta1 (Q 6) 'y' = (Q 1)
 delta1 (Q 6) 'z' = (Q 1)
 delta1 (Q 6) 'a' = (Q 3)
 delta1 (Q 6) 'e' = (Q 3)
 delta1 (Q 6) 'i' = (Q 3)
 delta1 (Q 6) 'o' = (Q 3)
 delta1 (Q 6) 'u' = (Q 3)
 delta1 (Q 6) 'l' = (Q 1)
 delta1 (Q 6) 'n' = (Q 1)
 delta1 (Q 6) 'r' = (Q 1)
 delta1 (Q 6) 's' = (Q 1)

 delta1 _ _ =  (Q 7)


 -- definimos el autómata
 fda1 = FDA {
         q = [(Q 0), (Q 1), (Q 2), (Q 3), (Q 4), (Q 5), (Q 6), (Q 7)],
         s = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','.'],
         d = delta1,
         q0 = (Q 0),
         f = [(Q 0),(Q 3),(Q 6)]
}