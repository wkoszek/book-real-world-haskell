module JSONParsec
    (
      p_text
    , parseFromFile
    ) where

import JSONClass
import Numeric (readFloat, readHex, readSigned)
import ApplicativeParsec

{-- snippet p_text --}
p_text :: CharParser () JValue
p_text = spaces *> text
     <?> "JSON text"
    where text = JObject <$> p_object
             <|> JArray <$> p_array
{-- /snippet p_text --}

{-- snippet p_value --}
p_value :: CharParser () JValue
p_value = value <* spaces
  where value = JString <$> p_string
            <|> JNumber <$> p_number
            <|> JObject <$> p_object
            <|> JArray <$> p_array
            <|> JBool <$> p_bool
            <|> JNull <$ string "null"
            <?> "JSON value"

p_bool :: CharParser () Bool
p_bool = True <$ string "true"
     <|> False <$ string "false"
{-- /snippet p_value --}

{-- snippet p_value_choice --}
p_value_choice = value <* spaces
  where value = choice [ JString <$> p_string
                       , JNumber <$> p_number
                       , JObject <$> p_object
                       , JArray <$> p_array
                       , JBool <$> p_bool
                       , JNull <$ string "null"
                       ]
                <?> "JSON value"
{-- /snippet p_value_choice --}

{-- snippet p_string --}
p_string :: CharParser () String
p_string = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (p_escape <|> p_unicode)
              <|> satisfy (`notElem` "\"\\")
{-- /snippet p_string --}

{-- snippet p_escape --}
p_escape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c
{-- /snippet p_escape --}

{-- snippet p_unicode --}
p_unicode :: CharParser () Char
p_unicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x
{-- /snippet p_unicode --}

{-- snippet p_number --}
p_number :: CharParser () Double
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
{-- /snippet p_number --}

{-- snippet p_series --}
p_series :: Char -> CharParser () a -> Char -> CharParser () [a]
p_series left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)
{-- /snippet p_series --}

{-- snippet p_object --}
p_object :: CharParser () (JObj JValue)
p_object = JObj <$> p_series '{' p_field '}'
    where p_field = (,) <$> (p_string <* char ':' <* spaces) <*> p_value
{-- /snippet p_object --}

{-- snippet p_array --}
p_array :: CharParser () (JAry JValue)
p_array = JAry <$> p_series '[' p_value ']'
{-- /snippet p_array --}
