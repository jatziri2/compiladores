module Ejercicio1 (
      parserChar
    , parser2charsEnUnpar)
where
import HuttonParsing

-----------------------

-- Definir uN parser para "Char's"
-- Ejemplo > parse parserChar "z"       -- input = "z"
--          DEBE producir [('z',"")]    -- resto del input= ""
-- Ejemplo > parse parserChar "wvz"       -- input = "wvz"
--          DEBE producir [('w',"vz")]    -- resto del input= "vz"

-- Un parser, parserChar, que analisa el input,
-- tiene exito si el input tiene un caracter (cualquiera),
-- y falla en caso contrario:
-- parserChar = P (\inp -> case inp of
--     []      -> [] -- el parser falla
--     (x:xs)  -> [(x,xs)]) -- [(a,String)] con x=Char, xs= resto del input
        -- lo que está entre parentesis es una funcion anonima (\inp .... )
parserChar :: Parser Char
parserChar = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion inp = case inp of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> [(x,xs)] -- [(a,String)] con x=Char, xs= resto del input

-- Otro ejemplo de parser, parser2charsEnUnpar
-- Analisa dos caracteres (cualesquiera) y los regresa en un par.
-- Ejemplo: parse parser2charsEnUnpar "abcd"
--          [( ('a','b'),  "cd")]
-- busca dos caracteres en el input y los regresa en la "estructura de un par", (_, _).

parser2charsEnUnpar :: Parser (Char, Char)
parser2charsEnUnpar = --P (Char,Char)
                    do -- para hacer la composicion "secuencial" de dos parsers.
                        x <- parserChar -- si tiene exito, en x está el resultado
                        y <- parserChar -- si tiene exito, en y está el resultado
                        return (x,y) -- regresa el resultado (si ambos tuevieron exito).

-- RECORDANDO:
-- un parser es "newtype Parser a = P (String -> [(a,String)])"
--      El constructor P recibe una funcion, String -> [(a,String)],
--      y regresa "un parser de tipo a".
--
-- Un parser, parserX, se aplica a un input, stringX, mediante la funcion "parse" (analisa).
-- Ejemplo: > parse parserChar "abc"
-- Regresa:     [('a',"bc")]    -- un de un par, con el resultado del analisis en el primer elemento
                                -- y el resto del input en el segundo elemento.
--

parser2charsEnUnparX1 :: Parser (Char, Char)
parser2charsEnUnparX1 = --
                    do -- para hacer la composicion "secuencial" de dos parsers.
                        x <- parserChar -- si tiene exito, en x está el resultado
                        --y <- parserChar -- si tiene exito, en y está el resultado
                        return (x,x) -- regresa el resultado (si ambos tuevieron exito).

-- parser de dos caracteres que se regresan en una Lista.
parser2charsEnLista :: Parser [Char]
parser2charsEnLista = --
                    do -- para hacer la composicion "secuencial" de dos parsers.
                        x <- parserChar -- si tiene exito, en x está el resultado
                        y <- parserChar -- si tiene exito, en y está el resultado
                        return [x,y] -- regresa el resultado (si ambos tuevieron exito).


-- three2 :: Parser (Char,Char)
-- three2 = do
--             x <- item
--             _ <- item
--             z <- item
--             return (x,z)
--
-- -- > parse three2 "abcdef"
-- -- [((’a’,’c’),"def")]
-- -- > parse three2 "ab"
-- -- []


---------------
-- parse :: Parser a -> String -> [(a,String)]
-- parse (P p) inp = p inp  -- p :: String -> [(a,String)]


-- The parserChar parser is the basic building block from which all other parsers
-- that consume characters from the input will ultimately be constructed. Its
-- behaviour is illustrated by the following two examples:

-- > parse item ""
--     []
-- > parse item "abc"  -- -- [(a,String)] con x=Char, xs= resto del input
--     [(’a’,"bc")]

-- Ejercicio. Definir un parser, parserDigito que analice "digitos" (0,1,2,...,9)
-- Ejemplo > parse parserDigito "z"  -- input = "z"
--          DEBE producir []    -- resto del input= "z"

-- Ejemplo > parse parserDigito "1zw"  -- input = "1zw"
--          DEBE producir ['1',"zw"]    -- resto del input= "zw"
-- ... codigo Hskell...
