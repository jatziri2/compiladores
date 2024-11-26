module Ejercicio3 (
--     -- Ejercicio1:
--       parserChar
--     , parser2charsEnUnpar
--     , parser2charsEnUnparX1
--     , parser2charsEnLista
--     -- Ejercicio2:
--       parserCeroOmasChars
--     , parserUnoOmasDigitos
--     , parserCeroOmasDigitos
--     , parserCeroOmasCharsReversa
    -- Ejercicio3:
      parserCHAR_a_OR_1
    )
where
import HuttonParsing
--import Data.Char -- importar algunas funciones relativas a Char
--
import Ejercicio1 -- Si es necesario algo
import Ejercicio2 -- Si es necesario algo

--
------------------------------------------------------------------------
------------------------------------------------------------------------

-- Ejercicio 3. (21 de agosto de 2024)
-- Vamos a ver como se define un parser (usando Hutton) para
-- una regla que tiene "alternativas" ( | ), como la regla S -> a | 1
-- Supongamos G es la gramática
--     (V,Sigma,R,S) donde:
--     V = {S}
--     Sigma = {a,1}
--     R = {(S,a), (S,1)} = {S->a, S->1}
--     S es la variable de inicio (Start variable)
-- Es decir, G=({S}, {a,1}, {S->a,S->1}, S)
--
-- ¿Qué lenguaje produce la gramatica G?
-- Recordar que Sigma* denota el conjunto de todas las palabras con alfabeto Sigma.
--  L(G)= {w in {a,1}* | S =>* w}
--      = {a,1} un lenguaje con solamente dos palabras, cada palabra con un solo símbolo.
--
--  Vamos a definir un parser para el lenguaje L(G),
--  poniendo atención en la gramatica que genera a L(G).
--

-- Para la regla S -> a, necesitamos un parser que tenga exito solamente si el input comienza con 'a'.
-- Un parser para 'a'
-- Tiene Exito solo si el input comienza con 'a'
parserCHARa :: Parser Char
parserCHARa = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x=='a'
                                      then
                                      [(x,xs)] -- exito
                                      else []  -- falla
--

-- Para la regla S -> 1, necesitamos un parser que tenga exito solamente si el input comienza con '1'.
-- Un parser para '1'
-- Tiene Exito solo si el input comienza con '1'
parserCHAR1 :: Parser Char
parserCHAR1 = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x=='1'
                                      then
                                      [(x,xs)] -- exito, regresa x y el resto xs
                                      else []  -- falla
--

-- Para la regla S -> a | 1, necesitamos un parser que tenga exito
-- solamente si el input comienza con 'a', o si el input comienza con 1.
-- Un parser para 'a' | '1'
-- Tiene exito solo si el input comienza con 'a' o comienza con '1'
parserCHAR_a_OR_1 :: Parser Char
parserCHAR_a_OR_1 = parserCHARa <|> parserCHAR1

-- Volvemos al enunciado del Ejercicio3 (ver arriba).
-- Recordar que Sigma* denota el conjunto de todas las palabras con alfabeto Sigma.
-- Si input in {a,1}*, es decir si input es una palabra con alfabeto {a,1},
-- entonces, al hacer parsing (parse) con el parser parserCHAR_a_OR_1 aplicado a input,
-- el resultado del parsing es "exito" solo si input in L(G).
-- parse parserCHAR_a_OR_1 input -- tiene exito solo si input in {a,1}*.
-- Pero L(G)={a,1} y (parse parserCHAR_a_OR_1 input) tiene "exito" solo si input in {a,1},
--      por lo tanto, parserCHAR_a_OR_1 es un parser del lenguaje L(G).


------------------------------------------------------------------------
------------------------------------------------------------------------

-- -- Proxima clase:
-- -- Antes de definir un parser para una gramatica G,
-- -- definir un tipo de datos para representar la gramatica G.
-- -- Para la gramatica del ejercicio 3, G=({S}, {a,1}, {S->a,S->1}, S),
-- -- podemos definir un tipo de datos en Haskell que la representa internamente:
--
-- -- Un tipo de datos para la gramatica G=({S}, {a,1}, {S->a,S->1}, S).
-- -- Reescribimos G con notacion BNF (Backus-Naur Form):
-- -- G ::= Caracter_a | Entero_1
-- --      donde: Caracter_a ::= 'a'  y  Entero_1 = 1
-- data G = Caracter_a Char
--        | Entero_1   Int
--        deriving (Eq,Show)
-- --
--
-- mkGcaracter_a :: Char -> G
-- mkGcaracter_a c = if c=='a'
--                 then Caracter_a c
--                 else error $ "mkGcaracter_a: Invalid char, c= " ++ show c
-- --
--
-- mkGentero_1 :: Int -> G
-- mkGentero_1 n = if n==1
--                 then Entero_1 n
--                 else error $ "Entero_1: Invalid int, n= " ++ show n
--



--------------------------------------------------------------------------



