{-- snippet module --}
module DList
    (
      DList
    , fromList
    , toList
    , empty
    , append
    , cons
    , dfoldr
    ) where
{-- /snippet module --}

import Data.Monoid (Monoid(..))

{-- snippet newtype --}
newtype DList a = DL {
      unDL :: [a] -> [a]
    }
{-- /snippet newtype --}

{-- snippet append --}
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-- /snippet append --}

{-- snippet appendP --}
append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)
{-- /snippet appendP --}

{-- snippet list --}
fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []
{-- /snippet list --}

{-- snippet common --}
empty :: DList a
empty = DL id

-- equivalent of the list type's (:) operator
cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)
{-- /snippet common --}

{-- snippet Functor --}
dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
    where go x xs = cons (f x) xs

instance Functor DList where
    fmap = dmap
{-- /snippet Functor --}

{-- snippet safeHead --}
safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                (y:_) -> Just y
                _ -> Nothing
{-- /snippet safeHead --}

{-- snippet Monoid --}
instance Monoid (DList a) where
    mempty = empty
    mappend = append
{-- /snippet Monoid --}
