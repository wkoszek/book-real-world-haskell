{-- snippet fmap --}
fmap id       ==  id
fmap (f . g)  ==  fmap f . fmap g
{-- /snippet fmap --}
