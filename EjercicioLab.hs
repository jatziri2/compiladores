module EjercicioLab where
import HuttonParsing
import Data.Char -- importar algunas funciones relativas a Char
--
-- import Ejercicio1 -- Si es necesario algo
-- import Ejercicio2 -- Si es necesario algo

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

-- Ejercicios para laboratorio.

-- 1. Agregar a los parsers del Ejercicio3 (parserCHARa,parserCHAR1,pars,parserCHAR_a_OR_1)
--    un parser, parserCHARmas, que tiene exito solamente si el input comienza con '+'
--

-- TOKENS
-- Observar, en el capítulo de Hutton, la función token :: Parser a -> Parser a.
--          Esta funcion, token, transforma un parser en otro parser que es un token.
--          Intuitivamente, token(parserX) es un parser que consume el "espacio blanco"
--          antes y despues de lo que consume parserX:
--             token :: Parser a -> Parser a
--             token p =   do
--                         space   -- espacio antes
--                         v <- p
--                         space   -- espacio después
--                         return v

-- 2. Usando la funcion de Hutton:
--     a) Transforme parserCHAR_a_OR_1 a un token, tokenCHAR_a_OR_1.
--     b) Transforme parserCHARmas a un token, tokenCHARmas
--     c) Defina un parser, parser_a_OR_1_Mas__a_OR_1, que tenga exito, con inputs
--         de la forma: "   a  +    1", "   1  +    a", "   1  +    1"

---------------------------------------------------

-- SOLUCION de los ejercicios 1, 2a, 2b, y 2c:

-- 1. Agregar a los parsers del Ejercicio3 (parserCHARa,parserCHAR1,pars,parserCHAR_a_OR_1)
--    un parser, parserCHARmas, que tiene exito solamente si el input comienza con '+'

parserCHARmas :: Parser Char
parserCHARmas = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x=='+'
                                      then
                                      [(x,xs)] -- exito, regresa x y el resto xs
                                      else []  -- falla
--

-- 2. Usando la funcion de Hutton:



--     a) Transforme parserCHAR_a_OR_1 a un token, tokenCHAR_a_OR_1.
--     b) Transforme parserCHARmas a un token, tokenCHARmas
--     c) Defina un parser, parser_a_OR_1_Mas__a_OR_1, que tenga exito, con inputs
--         de la forma: "   a  +    1", "   1  +    a", "   1  +    1"
-- USAMOS tokens y la notación "do" para combinar los tokens

-- a)
tokenCHAR_a_OR_1 = token parserCHAR_a_OR_1 

-- b)
tokenCHARmas = token parserCHARmas
-- c) "   a  +    1"
-- "a  +    1"
-- "+    1"
-- "1"
parser_a_OR_1_Mas__a_OR_1 :: Parser [Char]
parser_a_OR_1_Mas__a_OR_1 =
    do
        x <- tokenCHAR_a_OR_1 -- 'a' ('a', "  +    1")
        s <- tokenCHARmas -- '+' ('+', "    1")
        y <- tokenCHAR_a_OR_1 -- '1'  ('1', "")
        return ([x,s,y]) -- Recordar que x,s,y SON caracteres.

-- TESTS:
--
-- *EjercicioLab> --Tests sobre los inputs: "   a  +    1", "   1  +    a", "   1  +    1"
-- *EjercicioLab> parse parser_a_OR_1_Mas__a_OR_1  "   a  +    1"
-- [("a+1","")]
-- *EjercicioLab>
-- *EjercicioLab> parse parser_a_OR_1_Mas__a_OR_1  "   a  +    1  restoDelInput"
-- [("a+1","restoDelInput")]
-- *EjercicioLab>
-- *EjercicioLab> parse parser_a_OR_1_Mas__a_OR_1  "   1  +    a  restoDelInput"
-- [("1+a","restoDelInput")]
-- *EjercicioLab>
-- *EjercicioLab> parse parser_a_OR_1_Mas__a_OR_1  "   1  +    1  restoDelInput"
-- [("1+1","restoDelInput")]
-- *EjercicioLab>

-- Esquema "clásico" de un Lexer (analizador léxico) y un Parser (analizador sintáctico)
--
--                                          +-------+                             +--------+
--    Input(Texto=lista de caracteres) ---> | Lexer | ---> (Lista de tokens) ---> | Parser | ---> data_Type
--                                          +-------+                             +--------+
--
-- En el sentido clásico:
-- El Lexer es esencialmente un parser que analiza el input para determinar el léxico (palabras,ortografía,tokens).
-- Mientras que el parser analiza el input (lista de tokens) para determinar la composición de tokens (sintaxis).
--
-- El enfoque del curso no requiere la distinción de lexers.
--
--------------------------------------------------------------------------

-- MAS EJERCICIOS

-- Implementar un parser que reconozca un digito
parserDIGIT :: Parser Char
parserDIGIT = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x >= '0' && x <= '9'
                                      then
                                      [(x,xs)] -- exito
                                      else []  -- falla

-- Implementar un parser que reconozca dos digitos consecutivos
parserTwoDigits :: Parser String
parserTwoDigits = do
    a <- parserDIGIT -- "1 1a"
    b <- parserDIGIT -- 
    return [a,b]

-- Implementar un parser que reconozca cualquier letra minuscula
parserLOWER :: Parser Char
parserLOWER = P unaFuncion
    where
        unaFuncion :: String -> [(Char,String)]
        unaFuncion input = case input of
                        ""      -> [] -- el parser falla
                        (x:xs)  -> if x >= 'a' && x <= 'z'
                                      then
                                      [(x,xs)] -- exito
                                      else []  -- falla

-- Implementar un parser que reconozca una o mas letras minusculas consecutivas
parserWORD :: Parser String
parserWORD = some parserLOWER -- "aaaA"

-- Implementar un parser que reconozca una palabra seguida de un signo '+' (mas) y luego un digito.
parser_WORD_PLUS_DIGIT :: Parser (String, Char, Char) -- "hola + 5"
parser_WORD_PLUS_DIGIT = do
    a <- token parserWORD -- "hola + 5"  -> hola
    b <- token parserCHARmas -- "+ 5" -> "+"
    c <- token parserDIGIT -- "5" -> '5'
    return (a, b, c)

-- Implementar un parser que acepte un numero entero
parserINT :: Parser String
parserINT = some parserDIGIT -- 555555

-- Implementar un parser que acepte expresiones de la forma numero + numero
parserSIMPLE_EXPR :: Parser (String, Char, String)
parserSIMPLE_EXPR = do
    n1 <- token parserINT
    op <- token parserCHARmas
    n2 <- token parserINT
    return (n1, op, n2)