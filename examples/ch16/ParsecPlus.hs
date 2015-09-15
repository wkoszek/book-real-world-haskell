{-- snippet instance --}
instance MonadPlus (GenParser tok st) where
    mzero = fail "mzero"
    mplus = (<|>)
{-- /snippet instance --}
