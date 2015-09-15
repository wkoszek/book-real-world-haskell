import Data.Char (toUpper)

{-- snippet upcaseFirst --}
upcaseFirst (c:cs) = toUpper c -- forgot ":cs" here
{-- /snippet upcaseFirst --}

{-- snippet camelCase --}
camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
{-- /snippet camelCase --}
