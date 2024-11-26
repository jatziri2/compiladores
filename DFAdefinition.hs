module DFAdefinition
-- Un tipo de datos para DFAs (Automatas Finitos Deterministas)
--
    (
      DFA (..)          -- tipo de datos para DFAs
    , showDFA
    , tab4, tab8, tab12
    )
where
--
import Data.Set as S
    (Set
    , toList, fromList
    ) -- Finite Sets
    --, fromList, (\\), toList, empty, insert, intersection, member, unions
--
import Data.List
    (lookup)
    -- elem
--
-- import DFA (DFAstateSym(..)) FUE TEMPORAL
--
--import Data.Char -- importar algunas funciones relativas a Char
--
-- import HuttonParsing
--
--
--
------------------------------------------------------------------------
------------------------------------------------------------------------

-- Automatas Finitos Deterministas, DFAs (Deterministic Finite Automata): ----------------
--
-- DFA con parametros para estados y simbolos, Para el PARSER: -----------------------------------------
data DFA state symbol -- A = (Q, Sigma, delta, q0, F)
    = DFA -- Una tupla para M = (Q,S,e,Delta,S,F)
        ( String                            -- Nombre del DFA
        , S.Set state                       -- conjunto de estados Q
        , S.Set symbol                      -- conjunto de símbolos Sigma
        , S.Set ((state,symbol), state)     -- {((q,s), q') | q,q' in Q, s in Sigma} delta
                                            -- representa la funcion delta : (Q x S) -> Q
        , state                             -- estados inicial q0
        , S.Set state                       -- conjunto de estados de aceptación F
        )
        deriving (Show, Eq) -- heredando la funcion para mostrar, show, y la igualdad ==.
--
----------------------------------------------------
-- Tabuladores:

tab4 :: [Char]
-- tab4 = "\t"
tab4 = "    " -- 4 espacios
--
--
tab8 :: [Char]
tab8= "        " -- 8 espacios
--
tab12 :: [Char]
tab12= "            " -- 12 espacios
--

------------------------------------------------------
-- Una funcion para mostrar un DFA en forma mas legible que el default que nos da la clase Show
showDFA :: (Show state, Show symbol) => DFA state symbol -> [Char]
showDFA (DFA (nombreDFA, conjEstadosQ, conjSimbolos
                    , conjFdelta, edoInicial, conjEdosFinales))=
       tab4  ++ "DFA "   ++ nombreDFA        ++ " = (\n" -- Nombre
    ++ tab8 ++ "Q= "    ++ show estadosL    ++ "\n"     -- Estados Q
    ++ tab8 ++ "Sigma= "++ show simbolosL   ++ "\n"     -- Simbolos Sigma
    ++ tab8 ++ "delta= "++ show deltaL      ++ "\n"     -- delta
    ++ tab8 ++ "qInit= "++ show edoInicial  ++ "\n"     -- edo inicial qInit
    ++ tab8 ++ "F= "    ++ show edosFinL    ++ "\n"     -- edos Finales F
    ++ tab8 ++ " )\n "                                  -- parentesis final.
        where
        estadosL    = S.toList conjEstadosQ
        simbolosL   = S.toList conjSimbolos
        deltaL      = S.toList conjFdelta
        edosFinL    = S.toList conjEdosFinales
--
----------------------------------------------
--


-- Convertir una funcion delta
-- de la forma "S.Set ((state,symbol), state)"  <== Lo que entrega el parser
-- a la forma  "state -> symbol -> state"       <== Lo que necesita DFA.deltaGorro
-- para poder ejecutar el DFA.

deltaParesQxSxQtoFuncion ::
    S.Set ((String,String), String) -> (String -> String -> String)
deltaParesQxSxQtoFuncion tuplaQSQset
    = delta
    where
        delta :: (String -> String -> String)
        delta p s
            = case maybe_delta (p,s) of
                    Just q  -> q
                    Nothing -> error $ "deltaParesQxSxQtoFuncion: tuplas incompletas, "++" delta" ++ show (p,s) ++"=?"
        --
        maybe_delta (p,s) = lookup (p,s) tuplaQSQlist
                        -- ((p,s),q) ... delta p s = lookup (p,s) tuplaQSQlist
        tuplaQSQlist    = S.toList tuplaQSQset
--

-- -- Convierte un DFA del parser a un DFA de la ejecucion.
-- dfaParserTOdfaDefinicion
--         (DFA (nombreDFA, conjEstadosQ, conjSimbolos
--                     , conjFdelta, edoInicial, conjEdosFinales))
--     =   (DFAstateSym    -- M = (Q,S,delta,q0,F)
--                     ( xName     -- nombre del DFA
--                     , xQ        -- conjunto de estados
--                     , xSigma    -- conjunto de símbolos
--                     , delta     -- transicion, delta : (Q x S) -> Q
--                     , xI        -- estado inicial
--                     , xF        -- conjunto de estados de aceptación
--                     ) )
--     where
--     xName   = nombreDFA         -- nombre del DFA
--     xQ      = conjEstadosQ      -- conjunto de estados
--     xSigma  = conjSimbolos      -- conjunto de símbolos
--     delta   = deltaParesQxSxQtoFuncion conjFdelta -- delta
--     xI      = edoInicial        -- estado inicial
--     xF      = conjEdosFinales   -- conjunto de estados de aceptación
--

















-- Vamos a usar la funcion lookup de Data.List
-- DFAtoDot Data.List>
-- *DFAtoDot Data.List> listaParesLlaveAtributo = [(1,"a"),(2,"b")]
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List> lookup 2 listaParesLlaveAtributo
-- Just "b"
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List> lookup 1 listaParesLlaveAtributo
-- Just "a"
-- *DFAtoDot Data.List> listaQxSxQ = [(("p","a"),"q"), (("p","b"),"r")]
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List> lookup ("p","b") listaQxSxQ
-- Just "r"
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List> :t lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- *DFAtoDot Data.List>
-- *DFAtoDot Data.List> lookup ("p","C") listaQxSxQ
-- Nothing
-- *DFAtoDot Data.List>


-- transicion, delta : (Q x S) -> Q
-- {((q,s), q') | q,q' in Q, s in Sigma} delta
-- representa la funcion delta : (Q x S) -> Q













