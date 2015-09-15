{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-- snippet class --}
class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a
{-- /snippet class --}

{-- snippet instances --}
instance (Monad m) => Functor (ReaderT r m) where
    ...

instance (MonadIO m) => MonadIO (ReaderT r m) where
    ...

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    ...
{-- /snippet instances --}
