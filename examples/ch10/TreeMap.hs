import Prelude hiding (Functor(..))

{-- snippet Tree --}
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)
{-- /snippet Tree --}

{-- snippet treeLengths --}
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)
{-- /snippet treeLengths --}

{-- snippet treeMap --}
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)
{-- /snippet treeMap --}

{-- snippet Functor --}
class Functor f where
    fmap :: (a -> b) -> f a -> f b
{-- /snippet Functor --}

{-- snippet Functor.Tree --}
instance Functor Tree where
    fmap = treeMap
{-- /snippet Functor.Tree --}

{-- snippet Functor.List --}
instance Functor [] where
    fmap = map
{-- /snippet Functor.List --}

{-- snippet Functor.Maybe --}
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)
{-- /snippet Functor.Maybe --}
