{-- snippet instance --}
instance MonadPlus STM where
  mzero = retry
  mplus = orElse
{-- /snippet instance --}

{-- snippet msum --}
msum :: MonadPlus m => [m a] -> m a
msum =  foldr mplus mzero
{-- /snippet msum --}
