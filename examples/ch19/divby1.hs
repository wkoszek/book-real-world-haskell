{-- snippet all --}
divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)
{-- /snippet all --}
