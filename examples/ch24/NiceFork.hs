{-- snippet module --}
module NiceFork
    (
      ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where
{-- /snippet module --}

import Control.Monad (join)
{-- snippet header --}
import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished         -- terminated normally
                  | Threw Exception  -- killed by uncaught exception
                    deriving (Eq, Show)

-- | Create a new thread manager.
newManager :: IO ThreadManager

-- | Create a new managed thread.
forkManaged :: ThreadManager -> IO () -> IO ThreadId

-- | Immediately return the status of a managed thread.
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until a specific managed thread terminates.
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

-- | Block until all managed threads terminate.
waitAll :: ThreadManager -> IO ()
{-- /snippet header --}

{-- snippet ThreadManager --}
newtype ThreadManager =
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

newManager = Mgr `fmap` newMVar M.empty
{-- /snippet ThreadManager --}

{-- snippet forkManaged --}
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
      state <- newEmptyMVar
      tid <- forkIO $ do
        result <- try body
        putMVar state (either Threw (const Finished) result)
      return (M.insert tid state m, tid)
{-- /snippet forkManaged --}

forkManaged_racy (Mgr mgr) body = do
  state <- newEmptyMVar
  tid <- forkIO $
    try body >>= putMVar state . either Threw (const Finished)
  modifyMVar_ mgr (return . M.insert tid state)  -- here's the race
  return tid

{-- snippet getStatus --}
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> tryTakeMVar st >>= \mst -> case mst of
                   Nothing -> return (m, Just Running)
                   Just sth -> return (M.delete tid m, Just sth)
{-- /snippet getStatus --}

{-- snippet waitFor_bad --}
waitFor_bad (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> return (m, Nothing)
      (Just done, m') -> takeMVar done >>= \st -> return (m', Just st)
{-- /snippet waitFor_bad --}

{-- snippet waitFor --}
waitFor (Mgr mgr) tid = do
  maybeDone <- modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, Nothing)
      (done, m') -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st
{-- /snippet waitFor --}

{-- snippet waitFor2 --}
waitFor2 (Mgr mgr) tid =
  join . modifyMVar mgr $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just `fmap` takeMVar st)
{-- /snippet waitFor2 --}

{-- snippet waitAll --}
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)
{-- /snippet waitAll --}
