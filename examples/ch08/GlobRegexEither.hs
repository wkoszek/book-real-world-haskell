module GlobRegexEither
    (
      globToRegex
    , matchesPattern
    ) where

import Text.Regex.Posix ((=~))

regexChars :: [Char]
regexChars = "+()^$.{}]|"

{-- snippet type --}
type GlobError = String

globToRegex :: String -> Either GlobError String
{-- /snippet type --}

globToRegex = globToRegex' []

globToRegex' :: [Char] -> String -> Either GlobError [Char]

globToRegex' acc ('*':cs) = globToRegex' ("*." ++ acc) cs
globToRegex' acc ('?':cs) = globToRegex' ('.':acc) cs
globToRegex' acc ('[':'!':c:cs) = charClass ("^[" ++ acc) cs
globToRegex' acc ('[':c:cs) = charClass ('[':acc) cs
globToRegex' acc ('[':_) = Left "unterminated character class"
globToRegex' acc (c:cs)
    | c `elem` regexChars = globToRegex' (c:'\\':acc) cs
    | otherwise = globToRegex' (c:acc) cs
globToRegex' acc "" = Right (reverse ('$':acc))

charClass :: [Char] -> String -> Either GlobError [Char]

charClass acc (']':cs) = globToRegex' (']':acc) cs
charClass acc (c:cs) = charClass (c:acc) cs
charClass acc [] = Left "unterminated character class"

matchesPattern :: String -> String -> Either GlobError Bool
name `matchesPattern` pat = case globToRegex pat of
                            Left err -> Left err
                            Right regex -> Right (name =~ regex)
