import Control.Monad (ap, liftM2)
import Text.ParserCombinators.Parsec
import Numeric (readHex)

{-- snippet p_query --}
p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'
{-- /snippet p_query --}

{-- snippet p_pair --}
p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)
{-- /snippet p_pair --}

{-- snippet p_pair_app1 --}
p_pair_app1 =
    liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))
{-- /snippet p_pair_app1 --}

{-- snippet p_pair_app2 --}
p_pair_app2 =
    (,) `fmap` many1 p_char `ap` optionMaybe (char '=' >> many p_char)
{-- /snippet p_pair_app2 --}

{-- snippet p_char --}
p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d
{-- /snippet p_char --}
