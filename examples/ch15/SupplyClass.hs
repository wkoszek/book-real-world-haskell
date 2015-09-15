{-- snippet module --}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies,
             MultiParamTypeClasses #-}

module SupplyClass
    (
      MonadSupply(..)
    , S.Supply
    , S.runSupply
    ) where
{-- /snippet module --}

{-- snippet instance --}
import qualified Supply as S

instance MonadSupply s (S.Supply s) where
    next = S.next
{-- /snippet instance --}

{-- snippet class --}
class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
{-- /snippet class --}

{-- snippet showTwo_class --}
showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
{-- /snippet showTwo_class --}
