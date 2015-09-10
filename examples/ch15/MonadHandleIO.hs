{-- snippet IO --}
{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

import MonadHandle
import qualified System.IO

import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)

import SafeHello

instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStr = System.IO.hPutStr
    hClose = System.IO.hClose
    hGetContents = System.IO.hGetContents
    hPutStrLn = System.IO.hPutStrLn
{-- /snippet IO --}

{-- snippet tidyHello --}
tidyHello :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
{-- /snippet tidyHello --}

{-- snippet tidierHello --}
class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO System.IO.Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
  safeHello path
  liftIO (removeFile path)
{-- /snippet tidierHello --}

newtype HandleT m a = HandleT { runHandleT :: m a }

instance MonadTrans HandleT where
    lift = HandleT

instance Monad m => Monad (HandleT m) where
    return = HandleT . return
    m >>= k = HandleT $ runHandleT m >>= runHandleT . k
    fail = lift . fail

instance MonadIO m => MonadIO (HandleT m) where
    liftIO = lift . liftIO
