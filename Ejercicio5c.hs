module Ejercicio5c (
-- Comenzando un parser para DFAs (Automatas Finitos Deterministas)
      DFA (..)
    , parserQestados  -- con tokens
    , parserQestadosSINtokens
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

-- Automatas Finitos Deterministas, DFAs (Deterministic Finite Automata): ----------------
--
-- DFA con parametros para estados y simbolos: -----------------------------------------
data DFA state symbol -- A = (Q, Sigma, delta, q0, F)
    = DFA -- Una tupla para M = (Q,S,e,Delta,S,F)
        ( String                            -- Nombre del DFA
        , S.Set state                       -- conjunto de estados Q
        , S.Set symbol                      -- conjunto de símbolos Sigma
        , S.Set ((state,symbol), state)     -- {((q,s), q') | q,q' in Q, s in Sigma} delta
        --, state -> symbol -> state
        -- transicion, delta : (Q x S) -> Q
        , state                             -- estados inicial q0
        , S.Set state                       -- conjunto de estados de aceptación F
        )
        deriving (Show, Eq)
--
--

-- showDFA FALTA.


-- OBJETIVO: un parser para DFAs
-- tipo del input:
-- "A1 = (Q={q0 q1} Sigma={0 1} delta={(q0 0) q1) (q0 1) q0)} q0 F={q1})"

-- Empezamos con un parser para algo del tipo "Q={q0 q1}"

-- parser para "Q="

-- parserQigual :: Parser String
-- parserQigual = string "Q="
-- -- string es una funcion que, dada str,
-- -- construye un parser que tiene exito solo si el input comienza con el el string str

tokenQ :: Parser Char
tokenQ = token (char 'Q')

tokenEq :: Parser Char
tokenEq = token (char '=')

parserLlaveIzq :: Parser Char
parserLlaveIzq = char '{' -- char es una funcion que, dada un caracter,
-- construye un parser que tiene exito solo si el input comienza con dicho caracter.

parserLlaveDer :: Parser Char
parserLlaveDer = char '}' -- char es una funcion que, dada un caracter,
-- construye un parser que tiene exito solo si el input comienza con dicho caracter.

--parser para <estado> ::= q<digitos>+  -- q seguido de uno o mas digitos.
-- En forma equivalente (SIN usar X^+)
-- <unoOmasDigitos> ::=  Digito | Digito <unoOmasDigitos>
--parser para <estado> ::= q   -- q seguido de uno o mas digitos.

-- Otro parser para uno o mas digitos
-- NO funciona REVISAR. <===========================
-- someDigit :: Parser [Char]
-- someDigit =
--     do
--         d <- digit
--         return [d]
--     <|>
--     do
--         d   <- digit
--         ld  <- someDigit
--         return (d : ld)

parserQestadosSINtokens :: Parser (Set [Char])
parserQestadosSINtokens = -- SIN tokens:
    do
        _           <- parserLlaveIzq -- deshechamos {
        listaEdos   <- (some parserEstado)
        _           <- parserLlaveDer
        return (fromList listaEdos)


--
--------------------------------------------------------------------------
-- Clase 28ago2024:

-- Necesitamos que partes del input se comporten como "tokens"
-- Def. Un token es un parser con "cero o mas blancos" antes y despues del parser.
--      Donde un "blanco" es cualquier carater que se despleiga como blanco en un texto.
--      Por ejemplo, espacio en blanco, tabuladores, caracters para cambio de línea.

-- Hutton nos da una funcion, token, que convierte un parser en un token.

-- Para definir un token para "{", usando la funcion que transforma un parser en un token:

tokenLlaveIzq :: Parser Char
tokenLlaveIzq = token parserLlaveIzq

tokenLlaveDer :: Parser Char
tokenLlaveDer = token parserLlaveDer

parserEstado :: Parser [Char]
parserEstado =
    do
        char_q          <- char 'q'
        unoOmasDigitos  <- (some digit) -- digit= parser para un digito.
        return (char_q:unoOmasDigitos)

tokenEstado :: Parser [Char]
tokenEstado = token parserEstado


-- Un parser para un conjunto de estados (usando tokens:
parserQestados :: Parser (Set [Char])
parserQestados = -- con tokens:
    do
        _           <- tokenQ -- deshechamos 'Q'
        _           <- tokenEq -- deshechamos '='
        _           <- tokenLlaveIzq -- deshechamos {
        listaEdos   <- (some tokenEstado) -- uno o mas tokens de estado
        _           <- tokenLlaveDer -- deshechamos }
        return (fromList listaEdos) -- regresa el conjunto de listaEdos


------------------------------------------------------------------------
------------------------------------------------------------------------
--

-- OBJETIVO: un parser para DFAs
-- tipo del input:
-- "A1 = (Q={q0 q1} Sigma={0 1} delta={(q0 0) q1) (q0 1) q0)} q0 F={q1})"

parserNombreDFA :: Parser [Char]
parserNombreDFA =
    do
        char_d          <- char 'A'
        unoOmasDigitos  <- (some digit) -- uno o mas digitos.
        return (char_d : unoOmasDigitos)

tokenNombreDFA :: Parser [Char]
tokenNombreDFA = token parserNombreDFA

tokenParenIzq :: Parser Char
tokenParenIzq = token (char '(' )

tokenParenDer :: Parser Char
tokenParenDer = token (char ')' )

-- "A1 = (Q={q0 q1} Sigma={0 1} delta={(q0 0) q1) (q0 1) q0)} q0 F={q1})"
parserDFA =
    do
        nombreDFA       <- tokenNombreDFA   --1
        _               <- tokenEq          --
        _               <- tokenParenIzq    --
        conjEstadosQ    <- parserQestados   --2
        -- ... FALTA
        edoInicial      <- tokenEstado      --5
        -- .... FALTA
        _               <- tokenParenDer
        -- DFA (String, Set state, Set symbol,
        --     Set ((state, symbol), state), state, Set state)
        return (DFA (nombreDFA, conjEstadosQ, S.empty
                    , S.empty, edoInicial, S.empty))     -- temporal XXX



