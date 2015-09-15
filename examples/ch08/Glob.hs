{-- snippet module --}
module Glob (namesMatching) where
{-- /snippet module --}

{-- snippet import.rest --}
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
{-- /snippet import.rest --}
{-- snippet import.directory --}
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
{-- /snippet import.directory --}
{-- snippet import.filepath --}
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))
{-- /snippet import.filepath --}

{-- snippet type --}
namesMatching :: String -> IO [FilePath]
{-- /snippet type --}

{-- snippet namesMatching --}
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
{-- /snippet namesMatching --}

{-- snippet namesMatching2 --}
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)
{-- /snippet namesMatching2 --}

{-- snippet listMatches --}
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (const (return [])) $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _       = False
{-- /snippet listMatches --}

{-- snippet listPlain --}
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
{-- /snippet listPlain --}

{-- snippet doesNameExist --}
doesNameExist :: FilePath -> IO Bool

doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
      then return True
      else doesDirectoryExist name
{-- /snippet doesNameExist --}
