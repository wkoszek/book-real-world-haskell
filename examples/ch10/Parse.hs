module Parse
    (
      ParseState(..)
    , Parse(..)
    , (==>)
    , (==>&)
    , parseByte
    , parseNat
    , skipSpaces
    , assert
    , w2c
    , parseWhile
    , parseWhileWith
    , identity
    , parse
    , parseRawPGM
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

import PNM (Greymap(..))

{-- snippet ParseState --}
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)
{-- /snippet ParseState --}

{-- snippet simpleParse --}
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined
{-- /snippet simpleParse --}

{-- snippet betterParse --}
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined
{-- /snippet betterParse --}

{-- snippet Parse --}
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
{-- /snippet Parse --}

{-- snippet identity --}
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
{-- /snippet identity --}

{-- snippet bail --}
bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err
{-- /snippet bail --}

{-- snippet bind --}
(==>) :: Parse a -> (a -> Parse b) -> Parse b

firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState
{-- /snippet bind --}

{-- snippet parse --}
parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result
{-- /snippet parse --}

{-- snippet Monad --}
instance Monad Parse where
    return = identity
    (>>=) = (==>)
    fail = bail
{-- /snippet Monad --}

{-- snippet getPut --}
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
{-- /snippet getPut --}

{-- snippet modifyOffset --}
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }
{-- /snippet modifyOffset --}

{-- snippet parseByte --}
-- import the Word8 type from Data.Word
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1
{-- /snippet parseByte --}

{-- snippet peekByte --}
peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState
{-- /snippet peekByte --}

{-- snippet Functor --}
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)
{-- /snippet Functor --}

{-- snippet peekChar --}
peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte
{-- /snippet peekChar --}

{-- snippet parseChar --}
w2c :: Word8 -> Char
w2c = chr . fromIntegral

-- import Control.Applicative
parseChar :: Parse Char
parseChar = w2c <$> parseByte
{-- /snippet parseChar --}

{-- snippet parseWhile --}
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []
{-- /snippet parseWhile --}

{-- snippet parseWhileVerbose --}
parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
      Nothing -> identity []
      Just c | p c ->
                 parseByte ==> \b ->
                 parseWhileVerbose p ==> \bs ->
                 identity (b:bs)
             | otherwise ->
                 identity []
{-- /snippet parseWhileVerbose --}

{-- snippet helpers --}
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err
{-- /snippet helpers --}

{-- snippet parseBytes --}
parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
{-- /snippet parseBytes --}

{-- snippet parseRawPGM --}
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
{-- /snippet parseRawPGM --}
