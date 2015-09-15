{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses,
    TypeSynonymInstances #-}

module WriterIO where

import Control.Monad.Writer
import MonadHandle
import System.IO (IOMode(..))
import SafeHello

{-- snippet Event --}
data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)
{-- /snippet Event --}

{-- snippet WriterIO --}
newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])
{-- /snippet WriterIO --}

{-- snippet runWriterIO --}
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
{-- /snippet runWriterIO --}

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""
