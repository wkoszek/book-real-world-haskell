module Tree where

{-- snippet Tree --}
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

{-- /snippet Tree --}

{-- snippet simpleTree --}
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)
{-- /snippet simpleTree --}
