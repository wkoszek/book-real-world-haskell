{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
    MultiParamTypeClasses, TypeSynonymInstances #-}

module URLParser
    (
      URLParser
    , end
    , part
    , (/>)
    , (</)
    , (*/)
    ) where

import ApplicativeParsec

type URLParser a = CharParser () a

notSep :: URLParser Char
notSep = noneOf "/?#\r\n"

part :: URLParser String
part = many1 notSep

class URLPart a b | a -> b where
    urlPart :: a -> URLParser b

instance URLPart Char Char where
    urlPart c = char c

instance URLPart String String where
    urlPart s = string s

instance URLPart (URLParser a) a where
    urlPart p = p

(/>) :: (URLPart a _z, URLPart b c) => a -> b -> URLParser c
a /> b = urlPart a *> char '/' *> urlPart b
infixl 4 />

(</) :: (URLPart a c, URLPart b _z) => a -> b -> URLParser c
a </ b = urlPart a <* char '/' <* urlPart b
infixl 4 </

(*/) :: URLParser (a -> b) -> URLParser a -> URLParser b
a */ b = (urlPart a <* char '/') <*> urlPart b
infixl 4 */

end :: URLParser ()
end = optional (char '/') *> eof
