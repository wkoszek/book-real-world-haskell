{-- snippet returnSingleton --}
returnSingleton :: a -> [a]
returnSingleton x = [x]
{-- /snippet returnSingleton --}

{-- snippet instance --}
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
{-- /snippet instance --}
{-- snippet rest --}
    xs >> f = concat (map (\_ -> f) xs)
    fail _ = []
{-- /snippet rest --}
