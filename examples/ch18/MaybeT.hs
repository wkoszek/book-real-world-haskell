{-- snippet LANGUAGE --}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}
{-- /snippet LANGUAGE --}

module MaybeT
    (
      MaybeT
    , runMaybeT
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

{-- snippet newtype --}
newtype MaybeT m a = MaybeT {
      runMaybeT :: m (Maybe a)
    }
{-- /snippet newtype --}
 
{-- snippet bindMT.type --}
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
{-- /snippet bindMT.type --}

{-- snippet bindMT --}
x `bindMT` f = MaybeT $ do
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y -> runMaybeT (f y)
{-- /snippet bindMT --}

{-- snippet altBindMT --}
x `altBindMT` f =
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
{-- /snippet altBindMT --}

{-- snippet Monad --}
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing
 
instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT
{-- /snippet Monad --}

{-- snippet MonadTrans --}
instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)
{-- /snippet MonadTrans --}

instance (Functor m) => Functor (MaybeT m) where
  fmap f x = MaybeT $ fmap (fmap f) . runMaybeT $ x

{-- snippet mtl --}
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)

-- ... and so on for MonadReader, MonadWriter, etc ...
{-- /snippet mtl --}

instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  listen m = MaybeT $ do
               (result,log) <- listen (runMaybeT m)
               case result of
                 Nothing -> return Nothing
                 Just value -> return (Just (value,log))
  pass m = MaybeT $ do
             result <- runMaybeT m
             case result of
               Nothing -> return Nothing
               Just (value,log) -> pass (return (Just value,log))
