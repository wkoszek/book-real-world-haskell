import Control.Monad (MonadPlus(..))

shortcircuitLeft f =
{-- snippet shortcircuitLeft --}
    mzero >>= f == mzero
{-- /snippet shortcircuitLeft --}
      
shortcircuitRight v =
{-- snippet shortcircuitRight --}
    v >> mzero == mzero
{-- /snippet shortcircuitRight --}

{-- snippet guard --}
guard        :: (MonadPlus m) => Bool -> m ()
guard True   =  return ()
guard False  =  mzero
{-- /snippet guard --}

{-- snippet zeroMod --}
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x
{-- /snippet zeroMod --}
