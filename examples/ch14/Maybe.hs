import Prelude hiding (Maybe(..), Monad(..))

{-- snippet Maybe --}
data Maybe a = Nothing
             | Just a
{-- /snippet Maybe --}
               deriving (Eq, Ord, Show)

{-- snippet chain --}
chain :: m a -> (a -> m b) -> m b
{-- /snippet chain --}
chain = undefined

{-- snippet inject --}
inject :: a -> m a
{-- /snippet inject --}
inject = undefined


{-- snippet Monad --}
class Monad m where
    -- chain
    (>>=)  :: m a -> (a -> m b) -> m b
    -- inject
    return :: a -> m a
{-- /snippet Monad --}

{-- snippet bind_ --}
    (>>) :: m a -> m b -> m b
    a >> f = a >>= \_ -> f
{-- /snippet bind_ --}

{-- snippet fail --}
    fail :: String -> m a
    fail = error
{-- /snippet fail --}

{-- snippet instance --}
instance Monad Maybe where
    Just x >>= k  =  k x
    Nothing >>= _ =  Nothing

    Just _ >> k   =  k
    Nothing >> _  =  Nothing

    return x      =  Just x

    fail _        =  Nothing
{-- /snippet instance --}

{-- snippet maybe --}
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x
{-- /snippet maybe --}
