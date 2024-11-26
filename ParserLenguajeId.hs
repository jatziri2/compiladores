module ParserLenguajeId
where
import HuttonParsing

parser1 :: Parser Char
parser1 = (char '1')

parserEq :: Parser Char
parserEq = (char '=')

parserIdentidad :: Parser String
parserIdentidad = do
    char1 <- parser1
    inner <- parserIdentidad
    char2 <- parser1
    return (char1: inner ++ [char2])
    <|>
    do
        eq <- parserEq
        return [eq]