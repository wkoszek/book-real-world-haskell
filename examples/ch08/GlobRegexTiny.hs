globToRegex :: String -> String

globToRegex cs = '^' : globToRegex' cs

globToRegex' :: String -> String

globToRegex' "" = "$"
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
