import ControlledVisit (Info(..), getInfo, getUsefulContents, isDirectory)

import Control.Monad (liftM)
import Data.Char (toLower)
import System.FilePath ((</>), takeFileName, takeExtension)

{-- snippet Iterate --}
data Iterate seed = Done     { unwrap :: seed }
                  | Skip     { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
{-- /snippet Iterate --}

{-- snippet foldTree --}
foldTree :: Iterator a -> a -> FilePath -> IO a

foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed

    walk seed (name:names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed''        -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)
{-- /snippet foldTree --}

{-- snippet atMostThreePictures --}
atMostThreePictures :: Iterator [FilePath]

atMostThreePictures paths info
    | length paths == 3
      = Done paths
    | isDirectory info && takeFileName path == ".svn"
      = Skip paths
    | extension `elem` [".jpg", ".png"]
      = Continue (path : paths)
    | otherwise
      = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info
{-- /snippet atMostThreePictures --}

{-- snippet countDirectories --}
countDirectories count info =
    Continue (if isDirectory info
              then count + 1
              else count)
{-- /snippet countDirectories --}

{-- snippet secondTake --}
secondTake paths info =
    case undefined of
    _ | isDirectory info && takeBaseName path == ".svn"
          -> Skip paths
      | extension `elem` ["jpg", "png"] && length paths' == 3
          -> Done paths'
      | otherwise
          -> Continue paths'
  where extension = map toLower (takeExtension path)
        path = infoPath info
        paths' = path : paths
{-- /snippet secondTake --}
