module DFAparserLenguajeId
-- Ejercicio. Un parser para el lenguaje
--   Id={1^n=1^n | n in Naturales},
--            El alfabeto, de simbolos terminales, es Sigma= {1,=}
--            El conjunto de variables (simbolos) NO-terminales es {<Identidad>}.
--
-- OBS. El lenguaje Id es un lenguaje tipico entre los CFLs (context free languages).
-- Teoricamente, para los lenguajes en CFL podemos hacer parsing
-- mediante un PDA (pushdown automata) no-determinista.
-- Esto es "teoricamente".
-- En la práctica, no existe ninguna computadora
-- que tenga el mismo poder que un PDA.
-- Tampoco existe una computadora
-- que tenga el mismo poder que una TM (Turing machine).
-- La razón es: NO existe en la práctica una memoria infinita.
--
-- Una gramatica, G_Id, para Id es (simboloInicial= <Identidad>)
--      <Identidad> ::= lambda=lambda | 1<Identidad>1
-- Omitiendo lambda, la gramatica, G_Id, es, con notacion BNF:
--      <Identidad> ::=  = | 1<Identidad>1
--      La gramatica G_Id es G_Id=(Sigma,V,S,P) con:
--                              Sigma= {1,=}
--                              V= {<Identidad>}
--                              S= <Identidad>
--                              P= {r1,r2}, con r1,r2 def. abajo.
--      Las reglas de produccion, P, de G_Id=(Sigma,V,S,P), son:
--      <Identidad> --> =               regla r1
--      <Identidad> --> 1<Identidad>1   regla r2
--
-- Si queremos producir "1=1" con la gramatica G_id:
--  <Identidad> -->(r2) 1<Identidad>1  -->(r1) 1=1
--
--
-- Ejemplos de <Identidad>:
--      1. lambda=lambda, i.e. =, que representa 0=0 con 0 in N.
--      2.  1=1,  , 1=1
--      3   11=11 , que representa 2=2
--
-- Un ejemplo de lenguaje más "aritmetico" que Id, sería
--      Suma = { 1^n+1^m=1^{n+m} | n,m in Naturales}
--    1+11=111 in Suma, 1^0+1^2=1^2 i.e lambda++1^2=1^2,
--    que en realidad representa 0+2=2
-- Del curso de Automatas, sabemos que Id puede reconocerse con NPDA (Id tiene una CFG),
--      pero Id NO puede reconocerse con un Automata Finito (DFA o NFA)
--
--
where
--
--
-- import Data.Set as S
--     ( Set
--     , fromList
--     ) -- Finite Sets
    --, toList, (\\), empty, insert, intersection, member, unions
--
-- import Data.List -- (elem)
--
-- import Data.Char -- importar algunas funciones relativas a Char
--
-- import DFAdefinition (DFA(..) )
--
import HuttonParsing
    ( Parser(..)
    , token
    , char
    , parse  -- para probar los parsers, parse parserX input.
--     , sat
--     , digit
--     , string
--     , some
    )
--
import Control.Applicative
--
--import Data.Char

--import DFAdataTypeArbolBin (ArbolBin(..))

--
----------------------------------------------------------------------
--

-- Una gramatica para Id es
--      <Identidad> ::= lambda=lambda | 1 <Identidad> 1
-- Si omitimos lambda, la gramatica es:
--      <Identidad> ::=  = | 1 <Identidad> 1
--
-- Ejemplos de <Identidad>:
--      1. lambda=lambda, i.e. =, que representa 0=0 con 0 in N.
--      2.  1=1,  , 1=1
--      3   11=11 , que representa 2=2
--
-- Un ejemplo de lenguaje más "aritmetico" que Id, sería
--      Suma = { 1^n+1^m=1^{n+m} | n,m in Naturales}
--    1+11=111 in Suma, 1^0+1^2=1^2 i.e lambda++1^2=1^2, que representa 0+2=2
-- Del curso de Automatas, sabemos que Id puede reconocerse con NPDA (Id tiene una CFG),
--      pero Id NO puede reconocerse con un Automata Finito (DFA o NFA)
--

data Id = CeroEqCero | Add1Izq1Der Id
        | Add1izq Id | Add1der Id  -- <== Lo AGREGAMOS para pruebas.
        deriving (Eq,Show)
--
-- 5 puntos de 100 en el examen 1 Tenoch Itzin Flores Rojas 3
--
showId :: Id -> [Char]
showId ident
    = case ident of
--      ceroEqCero  -> "="  -- ceroEqCero con "c..." es un patron "x"
        CeroEqCero          -> "="
        (Add1Izq1Der idx)   -> "1" ++ (showId idx) ++ "1"
        (Add1izq idx)       -> "1" ++ (showId idx)
        (Add1der idx)       -> (showId idx) ++ "1"

-- Para pruebas, una conversion de n=m al tipo de datos Id.
-- para input (n,m), regresar "1^n=1^m"
nIntmIntToId :: (Int,Int) -> Id
nIntmIntToId (n,m)
    -- Base de la recursion:
    | n<=0 && m<=0  = CeroEqCero
    -- Casos recursivos:
    | n>0  && m<=0  = Add1izq (nIntmIntToId (n-1,0))
    | n<=0 && m> 0  = Add1der (nIntmIntToId (0,m-1))
    | n> 0 && m> 0  = Add1Izq1Der (nIntmIntToId (n-1,m-1))
    | otherwise     = error $ "nIntmIntToId: algo RARO!"
--

--Tests:
-- showId $ nIntmIntToId (0,0)
-- *DFAparserLenguajeId> showId $ nIntmIntToId (9,9)
-- "111111111=111111111"
-- it :: [Char]
-- *DFAparserLenguajeId> showId $ nIntmIntToId (9,8)
-- "111111111=11111111"
-- it :: [Char]
-- *DFAparserLenguajeId> showId $ nIntmIntToId (9,0)
-- "111111111="
-- it :: [Char]
-- *DFAparserLenguajeId> showId $ nIntmIntToId (8,9)
-- "11111111=111111111"
-- it :: [Char]
-- *DFAparserLenguajeId> showId $ nIntmIntToId (0,9)
-- "=111111111"
-- it :: [Char]
-- *DFAparserLenguajeId> showId $ nIntmIntToId (-35,9)
-- "=111111111"
-- it :: [Char]
-- *DFAparserLenguajeId>
-- *DFAparserLenguajeId> showId $ nIntmIntToId (-35,0)
-- "="



-- -- Algunos parsers:
--

-- charTOparser es una funcion que, dada un caracter,
-- construye un parser que tiene exito solo si el input comienza con dicho caracter.
charTOparser :: Char -> Parser Char
charTOparser    = char   -- a la funcion "char" de Hutton la llamamos charTOparser

-- funcion que transforma un char en un token
charTOtoken :: Char -> Parser Char
charTOtoken c   = parserTOtoken (charTOparser c)


-- funcion que transforma un parser en token (Hutton)
parserTOtoken :: Parser a -> Parser a
parserTOtoken   = token  -- funcion que transforma un parser en token (Hutton)
--
--
-- ----------------------------------------------------------------------
--
--
-- Omitiendo lambda, la gramatica, G_Id, es, con notacion BNF:
--      <Identidad> ::=  = | 1<Identidad>1
--
--

-- -- Muestra:
-- Con input "1=1"
-- Otra muestra "11=11"

-- (parse parserLenguajeId input) debe producir:
--      Add1Izq1Der CeroEqCero
--
-- token1 :: Parser Char
-- token1 = charTOtoken '1'
-- tokenEq :: Parser Char
-- tokenEq = charTOtoken '='
--
parser1 :: Parser Char
parser1 = charTOparser '1'
--
parserEq :: Parser Char
parserEq = charTOparser '='

-- parser para cero o mas 1's, regres una lista de 1's, ['1','1']
ceroOmasUnos :: Parser [Char]
ceroOmasUnos = many parser1

-- La muestra "11=11"
parserLenguajeId :: Parser Id
parserLenguajeId =
    do
        -- Composicion de tres parsers:
        unosIzq <- ceroOmasUnos --1) 11   cero o mas 1's, ['1','1']
        _       <- parserEq     --2) =    simbolo =,      '='
        unosDer <- ceroOmasUnos --3) 11   cero o mas 1's, ['1','1']
        --
        -- Analizamos el resultado de dos parsers:
        let n= length unosIzq -- resultado del primer parser
        let m= length unosDer -- resultado del tercer parser
        -- el return depende del analisis del parsing:
        if n==m  -- if #(unos a la izq) = #(unos a la der)
            then
                -- el resultado del parsing esta en el lenguaje Id
                return (nIntmIntToId (n,m))
            else
                -- Error, el resultado del parsing no esta en Id.
                error $ "parserLenguajeId: error de parsing; "
                ++ "n="++ (show n) ++","++ "m="++ (show m)
                ++ " en 1^n=1^m"

-- tokenParRedIzq :: Parser Char
-- tokenParRedIzq = charTOtoken '('
--
-- tokenH :: Parser Char
-- tokenH = charTOtoken 'H'
--
-- -------------------------------
-- -- <Info> es un parametro.
-- -- Por ejemplo, <Info> = <Natural>:
-- --         <Natural> ::=  <DigitoNoCero> <Digito>^*  | 0
-- --         <DigitoNoCero> ::= 1 | 2 | 3 |...| 9,
-- --         <Digito> ::= 0 | 1 | 2 |...| 9
--
-- parserDigitoNoCero :: Parser Char
-- parserDigitoNoCero  = sat digitNOcero
--     where digitNOcero c = (isDigit c) && (c /= '0')
--
-- parserDigito :: Parser Char
-- parserDigito = sat isDigit
--
--
-- parserNatural :: Parser Int
-- parserNatural =
--    (do  -- natural que NO inicia con '0'
--     d  <- parserDigitoNoCero
--     ds <- many parserDigito
--     return (read (d:ds)::Int)
--     )
--     <|>
--    (do  -- natural que SI inicia con '0', solo 0
--     d <- charTOtoken '0'
--     return (read [d]::Int)
--     )
-- --
-- -- Tests:
-- -- parse parserNatural "123"    -- Exito
-- -- parse parserNatural "0123"   -- Exito [(0, "123")]
-- -- parse parserNatural "0xxx"   -- Exito
--
-- -------------------------------
--
-- -- parserInfo :: a
-- -- parserInfo = undefined
--
-- tokenComa :: Parser Char
-- tokenComa = charTOtoken ','
--
-- tokenParRedDer :: Parser Char
-- tokenParRedDer = charTOtoken ')'
--
-- parserArbolBin :: Parser a -> Parser (ArbolBin a)
-- parserArbolBin parserX = -- parserX es un "parser parametro"
--     (   do         -- Muestra: A (H 1, 3, H 2) -- ver arb1 arriba
--         _       <- tokenA                   -- A
--         _       <- tokenParRedIzq           -- (
--         arbIzq  <- parserArbolBin parserX   -- H 1
--         _       <- tokenComa                -- ,
--         info    <- tokenInfo                -- 3
--         _       <- tokenComa                -- ,
--         arbDer  <- parserArbolBin parserX   -- H 2
--         _       <- tokenParRedDer           -- )
--         return (A(arbIzq, info, arbDer))
--         )
--     <|>
--     (   do      -- Muestra: H 1 -- ver arb1
--         _       <- tokenH       -- H
--         info    <- tokenInfo    -- 1
--         return (H info)
--         )
--     where tokenInfo = (parserTOtoken parserX)
-- --
--
-- -- Tests:
-- -- Ejemplos en los que parserX es parserNatural:
-- -- parse (parserArbolBin parserNatural ) "A (H 1, 3, H 2)"
-- -- parse (parserArbolBin parserNatural ) "A (A (H 1, 3, H 2), 7, A (H 4, 6, H 5))"
-- -- parse (parserArbolBin parserNatural ) "A (A (A (H 1, 3, H 2), 7, A (H 4, 6, H 5)),  15,  A (A (H 8, 10, H 9), 14, A (H 11, 13, H 12)))"
-- --
-- -- Ejemplos en  los que parserX es :
-- ------------------------------------------------
--
--
