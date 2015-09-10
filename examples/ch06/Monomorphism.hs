{-- snippet myShow --}
myShow = show
{-- /snippet myShow --}

{-- snippet myShow2 --}
myShow2 value = show value

myShow3 :: (Show a) => a -> String
myShow3 = show
{-- /snippet myShow2 --}
