{-# LANGUAGE MultiParamTypeClasses #-}

{-- snippet CustomT --}
newtype CustomT m a = ...
{-- /snippet CustomT --}

{-- snippet mtl --}
instance MonadReader r m => MonadReader r (CustomT m) where
    ...

instance MonadIO m => MonadIO (CustomT m) where
    ...
{-- /snippet mtl --}
