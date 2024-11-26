module ParserAnBnCn()

where 
import HuttonParsing

zeroOrMoreAs :: Parser String 
zeroOrMoreAs = many (char 'a')

zeroOrMoreBs :: Parser String 
zeroOrMoreBs = many (char 'b')

zeroOrMoreCs :: Parser String
zeroOrMoreCs = many (char 'c')

parserAnBnCn :: Parser (String, String, String)
parserAnBnCn = do
    as <- zeroOrMoreAs
    bs <- zeroOrMoreBs
    cs <- zeroOrMoreCs
    if length as == length bs && length as == length bs
        then return (as,bs,cs)
        else empty