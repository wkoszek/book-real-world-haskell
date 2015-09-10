{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
    GeneralizedNewtypeDeriving #-}

module SupplyInstance where

import SupplyClass
import RandomSupply
import Control.Monad (liftM)

{-- snippet Reader --}
newtype Reader e a = R { runReader :: e -> a }
{-- /snippet Reader --}

{-- snippet Monad --}
instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r
{-- /snippet Monad --}

{-- snippet ask --}
ask :: Reader e e
ask = R id
{-- /snippet ask --}

{-- snippet MySupply --}
newtype MySupply e a = MySupply { runMySupply :: Reader e a }
    deriving (Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply $ do
             v <- ask
             return (Just v)

    -- more concise:
    -- next = MySupply (Just `liftM` ask)
{-- /snippet MySupply --}

{-- snippet runMS --}
runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
{-- /snippet runMS --}

{-- snippet xy --}
xy :: (Num s, MonadSupply s m) => m s
xy = do
  Just x <- next
  Just y <- next
  return (x * y)
{-- /snippet xy --}
