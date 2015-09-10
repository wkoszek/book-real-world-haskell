{-- snippet State --}
newtype State s a = State {
      runState :: s -> (a, s)
    }
{-- /snippet State --}

{-- snippet returnState --}
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)
{-- /snippet returnState --}

{-- snippet bindState --}
bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'
{-- /snippet bindState --}

{-- snippet Monad --}
instance Monad (State s) where
    return a = returnState a
    m >>= k = bindState m k
{-- /snippet Monad --}

{-- snippet getPut --}
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
{-- /snippet getPut --}
