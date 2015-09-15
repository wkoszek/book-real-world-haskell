module ControlledVisit
    (
      Info(..)
    , getInfo
    , getUsefulContents
    , isDirectory
    ) where

import Control.Monad (filterM, forM, liftM)
import Data.List (partition)
import Data.Maybe (isJust)
import System.Directory (Permissions(..), getDirectoryContents,
                         getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

{-- snippet Info --}
data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe ClockTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
{-- /snippet Info --}

{-- snippet getInfo --}
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
{-- /snippet getInfo --}

{-- snippet traverse.type --}
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
{-- /snippet traverse.type --}
{-- snippet traverse --}
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
{-- /snippet traverse --}

{-- snippet traverseVerbose --}
traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                             Nothing -> False
                             Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]
{-- /snippet traverseVerbose --}
