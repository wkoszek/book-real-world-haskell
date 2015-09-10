module HttpParser
    (
      HttpRequest(..)
    , Method(..)
    , p_query
    , p_request
    , parse
    ) where

import ApplicativeParsec
import Numeric (readHex)

type HttpParser a = CharParser () a

p_query :: HttpParser [(String, Maybe String)]
p_query = pair `sepBy` char '&'
  where pair = (,) <$> many1 safe <*> optional (char '=' *> many safe)
        safe = oneOf urlBaseChars
           <|> char '%' *> liftA2 decode hexDigit hexDigit
           <|> ' ' <$ char '+'
           <?> "safe"
        decode a b = case readHex [a,b] of
                       ((n,_):_) -> toEnum n
                       _ -> error "the impossible has occurred!"
        urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

crlf :: HttpParser ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: HttpParser Char
notEOL = noneOf "\r\n"

p_headers :: HttpParser [(String, String)]
p_headers = header `manyTill` crlf
  where header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        fieldName = (:) <$> letter <*> many fieldChar
        fieldChar = letter <|> digit <|> oneOf "-_"
        contents = liftA2 (++) (many1 notEOL <* crlf)
                               (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

data Method = Get | Post
          deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
      httpMethod :: Method
    , httpURL :: String
    , httpHeaders :: [(String, String)]
    , httpBody :: Maybe String
    } deriving (Eq, Show)

p_request :: HttpParser HttpRequest
p_request = q "GET" Get (pure Nothing)
        <|> q "POST" Post (Just <$> many anyChar)
  where q s c p = HttpRequest <$> (c <$ string s <* char ' ')
                              <*> url <*> p_headers <*> p
        url = manyTill notEOL (try httpTrailer) <* crlf
        httpTrailer = string " HTTP/1." <* oneOf "01"
