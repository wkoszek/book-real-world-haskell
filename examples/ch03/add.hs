{-- snippet add --}
add a b = a + b
{-- /snippet add --}

{-- snippet myNot --}
myNot True  = False
myNot False = True
{-- /snippet myNot --}

{-- snippet sumList --}
sumList (x:xs) = x + sumList xs
sumList []     = 0
{-- /snippet sumList --}
