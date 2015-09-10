{-- snippet mySecond --}
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)
{-- /snippet mySecond --}

{-- snippet safeSecond --}
safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
{-- /snippet safeSecond --}

{-- snippet tidySecond --}
tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
{-- /snippet tidySecond --}
