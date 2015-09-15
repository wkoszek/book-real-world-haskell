{-- snippet module --}
module PrettyJSON
    (
      jvalue
    ) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)
{-- /snippet module --}

{-- snippet jvalue --}
jvalue :: JValue -> Doc
jvalue (JBool True) = text "true"
jvalue (JBool False) = text "false"
jvalue JNull = text "null"
jvalue (JNumber num) = double num
jvalue (JString str) = string str
{-- /snippet jvalue --}
{-- snippet jvalue.array --}
jvalue (JArray ary) = series '[' ']' jvalue ary
{-- /snippet jvalue.array --}
{-- snippet jvalue.object --}
jvalue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name <> text ": " <> jvalue val
{-- /snippet jvalue.object --}

{-- snippet enclose --}
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
{-- /snippet enclose --}

{-- snippet hexEscape --}
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral (d - 0x10000)
  where d = fromEnum c
{-- /snippet hexEscape --}

{-- snippet smallHex --}
smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) '0') <> text h
    where h = showHex x ""
{-- /snippet smallHex --}

{-- snippet astral --}
astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff
{-- /snippet astral --}

{-- snippet string --}
string :: String -> Doc
string = enclose '\"' '\"' . hcat . map oneChar
{-- /snippet string --}

{-- snippet oneChar --}
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, '\\':[b])
{-- /snippet oneChar --}

{-- snippet series --}
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
{-- /snippet series --}
