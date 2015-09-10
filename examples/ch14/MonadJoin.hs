{-- snippet join --}
join :: Monad m => m (m a) -> m a
join x = x >>= id
{-- /snippet join --}
         
