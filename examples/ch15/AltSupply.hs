import Control.Monad.State

newtype Supply s a = S (State [s] a)

{-- snippet unwrapS --}
unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Monad (Supply s) where
    s >>= m = S (unwrapS s >>= unwrapS . m)
    return = S . return
{-- /snippet unwrapS --}
