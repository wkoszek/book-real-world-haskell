import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")

{-- snippet eol --}
eol = 
    do char '\n'
       char '\r' <|> return '\n'
{-- /snippet eol --}

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
