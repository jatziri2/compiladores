module ParserLenguajeId
where
import HuttonParsing

parser1 :: Parser Char
parser1 = (char '1')

parserEq :: Parser String
parserEq = do
    eq <- char '='
    return [eq]

parser_1_identidad_1 :: Parser String
parser_1_identidad_1 = do
    charL <- parser1
    iden <- parserIdentidad
    charR <- parser1
    return (charL : iden ++ [charR])
    
parserIdentidad :: Parser String
parserIdentidad = parser_1_identidad_1 <|> parserEq
