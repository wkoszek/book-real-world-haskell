{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UglyStack (AppConfig, AppState) where

import CountEntries (listDirectory)

{-- snippet AppData --}
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)
{-- /snippet AppData --}

{-- snippet App --}
type App = ReaderT AppConfig (StateT AppState IO)
{-- /snippet App --}

{-- snippet App2 --}
type App2 a = ReaderT AppConfig (StateT AppState IO) a
{-- /snippet App2 --}

{-- snippet runApp --}
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state
{-- /snippet runApp --}

{-- snippet constrainedCount --}
constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest
{-- /snippet constrainedCount --}

{-- snippet MyApp --}
newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState IO) a
    } deriving (Monad, MonadIO, MonadReader AppConfig,
                MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state
{-- /snippet MyApp --}

{-- snippet explicitGet --}
explicitGet :: App AppState
explicitGet = lift get
{-- /snippet explicitGet --}

{-- snippet implicitGet --}
implicitGet :: App AppState
implicitGet = get
{-- /snippet implicitGet --}

whatever :: App AppState
whatever = do
  a <- lift get
  return a
