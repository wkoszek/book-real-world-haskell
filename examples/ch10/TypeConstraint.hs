module TypeConstraint where

{-- snippet OrdStack --}
data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                             deriving (Show)
{-- /snippet OrdStack --}

{-- snippet isIncreasing --}
isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b     = isIncreasing rest
    | otherwise = False
isIncreasing _  = True
{-- /snippet isIncreasing --}

{-- snippet push --}
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
{-- /snippet push --}
