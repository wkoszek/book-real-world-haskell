import System.FilePath
{-- snippet simpleFind --}
import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

simpleFind p path = do
  names <- getRecursiveContents path
  return (filter p names)
{-- /snippet simpleFind --}
