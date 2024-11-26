module Ejercicio5 (
-- Comenzando un parser para DFAs (Automatas Finitos Deterministas)
    )
where
import HuttonParsing
--import Data.Char -- importar algunas funciones relativas a Char
--
import Data.Set as S (Set, fromList, toList, member, insert, unions, intersection, empty) -- Finite Sets
-- import Data.List (elem)

--
-- Ejercicios anteriores:
-- import Ejercicio1 -- Si es necesario algo
-- import Ejercicio2 -- Si es necesario algo
-- import Ejercicio3 -- Si es necesario algo

--
------------------------------------------------------------------------
------------------------------------------------------------------------

-- RECORDAR ejercicio pendiente.....
--Proxima clase:
-- Antes de definir un parser para una gramatica G,
-- definir un tipo de datos para representar la gramatica G.
-- Para la gramatica del ejercicio 3, G=({S}, {a,1}, {S->a,S->1}, S),
-- podemos definir un tipo de datos en Haskell que la representa internamente:

----------------------------------

-- Un tipo de datos para la gramatica G=({S}, {a,1}, {S->a,S->1}, S).
-- Reescribimos G con notacion BNF (Backus-Naur Form):
-- G ::= Caracter_a | Entero_1
--      donde: Caracter_a ::= 'a'  y  Entero_1 = 1
data G = Caracter_a Char
       | Entero_1   Int
       deriving (Eq,Show)
-- El tipo de datos G tiene dos constructores:
--      1. Caracter_a,  que usa un Char para contruir algo de tipo G
--      2. Entero_1,    que usa un Int  para contruir algo de tipo G

-- Constructores restringidos

mkGcaracter_a :: Char -> G
mkGcaracter_a c = if c=='a'
                then Caracter_a c
                else error $ "mkGcaracter_a: Invalid char, c= " ++ show c
--

mkGentero_1 :: Int -> G
mkGentero_1 n = if n==1
                then Entero_1 n
                else error $ "Entero_1: Invalid int, n= " ++ show n
--
-------------------------------------------------------------------------

-- Automatas Finitos Deterministas, DFAs (Deterministic Finite Automata): ----------------
--
-- DFA con parametros para estados y simbolos: -----------------------------------------
data DFAstateSym state symbol -- A = (Q, Sigma, delta, q0, F)
    = DFAstateSym -- Una tupla para M = (Q,S,e,Delta,S,F)
        ( S.Set state                       -- conjunto de estados Q
        , S.Set symbol                      -- conjunto de símbolos Sigma
        , S.Set ((state,symbol), state)     -- {((q,s), q') | q,q' in Q, s in Sigma} delta
        --, state -> symbol -> state
        -- transicion, delta : (Q x S) -> Q
        , state                             -- estados inicial q0
        , S.Set state                       -- conjunto de estados de aceptación F
        )
--


-- OBJETIVO: un parser para DFAs
-- tipo del input:
-- "(Q={q0 q1}, Sigma={0 1}, delta={(q0,0),q1), (q0,1),q0)}, q0, F={q1})"

-- Empezamos con un parser para algo del tipo "Q={q0,q1}"

-- parser para "Q="

parserQigual = string "Q="
-- string es una funcion que, dada str,
-- construye un parser que tiene exito solo si el input comienza con el el string str

parserLlaveIzq = char '{' -- char es una funcion que, dada un caracter,
-- construye un parser que tiene exito solo si el input comienza con dicho caracter.

parserLlaveDer = char '}' -- char es una funcion que, dada un caracter,
-- construye un parser que tiene exito solo si el input comienza con dicho caracter.

--parser para <estado> ::= q<digitos>+  -- q seguido de uno o mas digitos.

parserEstado =
    do
        char_q          <- char 'q'
        unoOmasDigitos  <- (some digit) -- digit= parser para un digito.
        return (char_q:unoOmasDigitos)


parserQestados =
    do
        _           <- parserLlaveIzq
        listaEdos   <- (some parserEstado)
        _           <- parserLlaveDer
        return (fromList listaEdos)


--
--------------------------------------------------------------------------


-- Ejercicio 4. (Ejercicio 3 con data type)
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
parserGcaracter_a :: Parser G
parserGcaracter_a = P unaFuncion
    where
        unaFuncion :: String -> [(G,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x=='a'
                                      then
                                      [(mkGcaracter_a x,xs)] -- exito
                                      else []  -- falla
--

-- Para la regla S -> 1, necesitamos un parser que tenga exito solamente si el input comienza con '1'.
-- Un parser para '1'
-- Tiene Exito solo si el input comienza con '1'
parserGentero_1 :: Parser G
parserGentero_1 = P unaFuncion
    where
        unaFuncion :: String -> [(G,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x=='1'
                                      then
                                      [(mkGentero_1 (read (x:[]) ::Int), xs)] -- exito, regresa x y el resto xs
                                      else []  -- falla
--

-- Para la regla S -> a | 1, necesitamos un parser que tenga exito
-- solamente si el input comienza con 'a', o si el input comienza con 1.
-- Un parser para 'a' | '1'
-- Tiene exito solo si el input comienza con 'a' o comienza con '1'
parserGcaracter_a_OR_Gentero_1 :: Parser G
parserGcaracter_a_OR_Gentero_1 = parserGcaracter_a <|> parserGentero_1

-- Volvemos al enunciado del Ejercicio3 (ver arriba).
-- Recordar que Sigma* denota el conjunto de todas las palabras con alfabeto Sigma.
-- Si input in {a,1}*, es decir si input es una palabra con alfabeto {a,1},
-- entonces, al hacer parsing (parse) con el parser parserGcaracter_a_OR_Gentero_1 aplicado a input,
-- el resultado del parsing es "exito" solo si input in L(G).
-- parse parserGcaracter_a_OR_Gentero_1 input -- tiene exito solo si input in {a,1}*.
-- Pero L(G)={a,1} y (parse parserGcaracter_a_OR_Gentero_1 input) tiene "exito" solo si input in {a,1},
--      por lo tanto, parserGcaracter_a_OR_Gentero_1 es un parser del lenguaje L(G).


------------------------------------------------------------------------
------------------------------------------------------------------------



