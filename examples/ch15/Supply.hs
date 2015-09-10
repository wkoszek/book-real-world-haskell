{-- snippet LANGUAGE --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-- /snippet LANGUAGE --}

{-- snippet module --}
module Supply
    (
      Supply
    , next
    , runSupply
    ) where
{-- /snippet module --}

{-- snippet Supply --}
import Control.Monad.State

newtype Supply s a = S (State [s] a)
{-- /snippet Supply --}
{-- snippet deriving --}
    deriving (Monad)
{-- /snippet deriving --}

{-- snippet next.type --}
next :: Supply s (Maybe s)
{-- /snippet next.type --}

{-- snippet runSupply.type --}
runSupply :: Supply s a -> [s] -> (a, [s])
{-- /snippet runSupply.type --}

{-- snippet code --}
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)

runSupply (S m) xs = runState m xs
{-- /snippet code --}

{-- snippet showTwo --}
showTwo :: (Show s) => Supply s String
showTwo = do
  a <- next
  b <- next
  return (show "a: " ++ show a ++ ", b: " ++ show b)
{-- /snippet showTwo --}
