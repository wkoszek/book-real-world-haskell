{-- snippet splitLines.type --}
splitLines :: String -> [String]
{-- /snippet splitLines.type --}

{-- snippet splitLines --}
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'
{-- /snippet splitLines --}

{-- snippet fixLines --}
fixLines :: String -> String
fixLines input = unlines (splitLines input)
{-- /snippet fixLines --}
