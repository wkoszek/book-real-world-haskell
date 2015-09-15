import ApplicativeParsec
import Numeric (readHex)

{-- snippet a_query --}
a_query :: CharParser () [(String, Maybe String)]
a_query = a_pair `sepBy` char '&'
{-- /snippet a_query --}

{-- snippet a_pair --}
a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
{-- /snippet a_pair --}

{-- snippet p_char --}
p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
{-- /snippet p_char --}

{-- snippet a_char --}
a_char = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> a_hex
{-- /snippet a_char --}

{-- snippet p_hex --}
p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d
{-- /snippet p_hex --}

{-- snippet a_hex --}
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
    where hexify a b = toEnum . fst . head . readHex $ [a,b]
{-- /snippet a_hex --}

{-- snippet hexify --}
hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head . readHex $ [a,b]
{-- /snippet hexify --}
