{-- snippet module --}
module PrettyJSON
    (
      renderJValue
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)
{-- /snippet module --}

{-- snippet renderJValue --}
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
{-- /snippet renderJValue --}
{-- snippet renderJValue.array --}
renderJValue (JArray ary) = series '[' ']' renderJValue ary
{-- /snippet renderJValue.array --}
{-- snippet renderJValue.object --}
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = string name
                          <> text ": "
                          <> renderJValue val
{-- /snippet renderJValue.object --}

{-- snippet enclose --}
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right
{-- /snippet enclose --}

{-- snippet hexEscape --}
hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c
{-- /snippet hexEscape --}

{-- snippet smallHex --}
smallHex :: Int -> Doc
smallHex x  = text "\\u"
           <> text (replicate (4 - length h) '0')
           <> text h
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
string = enclose '"' '"' . hcat . map oneChar
{-- /snippet string --}

{-- snippet pointyString --}
pointyString :: String -> Doc
pointyString s = enclose '"' '"' (hcat (map oneChar s))
{-- /snippet pointyString --}

{-- snippet oneChar --}
oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])
{-- /snippet oneChar --}

{-- snippet series --}
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close
                       . fsep . punctuate (char ',') . map item
{-- /snippet series --}
