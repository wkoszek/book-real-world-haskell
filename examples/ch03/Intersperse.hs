{-- snippet intersperse --}
intersperse :: a -> [[a]] -> [a]
{-- /snippet intersperse --}
intersperse a (x:xs@(_:_)) = x ++ a:intersperse a xs
intersperse a (x:_) = x
intersperse _ _ = []
