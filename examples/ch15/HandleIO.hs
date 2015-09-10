{-- snippet module --}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    (
      HandleIO
    , Handle
    , IOMode(..)
    , runHandleIO
    , openFile
    , hClose
    , hPutStrLn
    ) where
    
import System.IO (Handle, IOMode(..))
import qualified System.IO
{-- /snippet module --}
import System.Directory (removeFile)

{-- snippet MonadIO --}
import Control.Monad.Trans (MonadIO(..))

instance MonadIO HandleIO where
    liftIO = HandleIO
{-- /snippet MonadIO --}

{-- snippet newtype --}
newtype HandleIO a = HandleIO { runHandleIO :: IO a }
    deriving (Monad)
{-- /snippet newtype --}

{-- snippet actions --}
openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)
{-- /snippet actions --}

{-- snippet safeHello --}
safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
{-- /snippet safeHello --}

{-- snippet tidyHello --}
tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO (removeFile path)
{-- /snippet tidyHello --}
