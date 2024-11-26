module Ejercicio2 (
      parserChar
    , parser2charsEnUnpar)
where
import HuttonParsing
import Data.Char -- importar algunas funciones relativas a Char


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

---------------------------------------------------------------------------------------
-- Ejercicio 2.

-- Un parser, parserCeroOmasChars, que reconozca cero o más caracteres cualesquiera.
-- Por ejemplo, > parse parserCeroOmasChars "yatbpq"
--                DEBE producir [("yatbpq", "")] -- NO sobra nada
-- Por ejemplo, > parse parserCeroOmasChars ""
--                DEBE producir [("", "")] -- regresa el string nulo, y NO sobra nada

-- Del capitulo de Hutton:
-- Our next two parsers, many p and some p, apply a parser p as many
-- times as possible until it fails, with the result values from each successful
-- application of p being returned in a list. The difference between these
-- two repetition primitives is that many permits zero or more applications
-- of p, whereas some requires at least one successful application. For
-- example:
--
--     > parse (many digit) "123abc"
--     [("123","abc")]
--     > parse (many digit) "abc"
--     [("","abc")]
--     > parse (some digit) "abc"
--     []

-- (many parserChar) es un parser que tiene exito cuando parserChar tiene exito cero o mas veces.
-- (many parserChar) regresa una lista con elementos del tipo que regresa parserChar (en este caso, un Char)

parserCeroOmasChars :: Parser [Char]
-- parser para cero mas caracteres cualesquiera.
parserCeroOmasChars = (many parserChar) -- recordar que parserChar es un parser para un caracter cualesquiera.

-- Otro ejemplo:
parserCeroOmasCharsReversa :: Parser [Char]
-- parser para cero mas caracteres cualesquiera.
parserCeroOmasCharsReversa =  -- recordar que parserChar es un parser para un caracter cualesq.
        do
            lx <- (many parserChar)
            return (reverse lx)


--
-- Otro ejemplo con digitos:
--
-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do
--         x <- item
--         if p x then return x else empty

parserSatPredBool :: (Char -> Bool) -> Parser Char
parserSatPredBool predBool = do
        x <- parserChar
        if (predBool x)
           then return x    -- regresa x si (predBool x = True=
           else empty       -- Falla en caso contrario.

parserUnDigito :: Parser Char
-- Otro parser, parserUnDigito, que tiene exito si el primer caracter del input es un digito.
parserUnDigito = parserSatPredBool isDigit

-- ahora un parser para cero o mas digitos, parserCeroOmasDigitos:

parserCeroOmasDigitos :: Parser Int
parserCeroOmasDigitos =
    do
        digitosStr <- (many parserUnDigito)
        return (read digitosStr)

parserUnoOmasDigitos :: Parser Int
parserUnoOmasDigitos =
    do
        digitosStr <- (some parserUnDigito)
        return (read digitosStr)

-- Ejercicio: definir un parser, parserVarId, que tenga exito con los identificadores de variables:
-- Un identificador de variable es intuitivamente: un letra minuscula seguida de uno o mas digitos.
-- Ejemplos OK: v1, v34, v12345
-- Ejemplos NO-OK: v, _v34, V12345, vx1
-- parserVarId regresa un string.
-- parse parserVarId "v1=35" DEBE regresar [("v1", "=35")]
-- parse parserVarId "vx1=35" DEBE regresar []
-- Intentarlo en casa. (pueden preguntar por correo o en el grupo)

parserVarId :: Parser [Char]
parserVarId = do 
    v <- (parserSatPredBool isLower)
    nums <- (some digit)
    return ([v] ++ nums)





