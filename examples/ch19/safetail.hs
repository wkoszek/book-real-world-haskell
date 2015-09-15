{-- snippet all --}
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
{-- /snippet all --}
