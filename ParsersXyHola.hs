module ParsersXyHola 
where
import HuttonParsing(Parser(..), parse)

parserX :: Parser Char
parserX = P (\inp -> case inp of
    []      -> [] -- el parser falla
    (x:xs)  -> if x == 'X' 
                    then [(x,xs)] 
                    else [])

parserHola :: Parser String
parserHola = P (\inp -> case inp of
    []      -> [] -- el parser falla
    ('H':'o':'l':'a':rest)  -> [("Hola", rest)]
    x -> [])