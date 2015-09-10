{-- snippet List --}
data List a = Cons a (List a)
            | Nil
              deriving (Show)
{-- /snippet List --}

{-- snippet fromList --}
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil
{-- /snippet fromList --}
