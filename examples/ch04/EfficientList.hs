{-- snippet myDumbExample --}
myDumbExample xs = if length xs > 0
                   then head xs
                   else 'Z'
{-- /snippet myDumbExample --}

{-- snippet mySmartExample --}
mySmartExample xs = if not (null xs)
                    then head xs
                    else 'Z'

myOtherExample (x:_) = x
myOtherExample [] = 'Z'
{-- /snippet mySmartExample --}
