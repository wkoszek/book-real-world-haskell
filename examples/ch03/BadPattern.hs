module BadPattern where

{-- snippet badExample --}
badExample (x:xs) = x + badExample xs
{-- /snippet badExample --}

{-- snippet goodExample --}
goodExample (x:xs) = x + goodExample xs
goodExample _      = 0
{-- /snippet goodExample --}
