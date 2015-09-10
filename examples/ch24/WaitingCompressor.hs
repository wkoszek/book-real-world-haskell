{-- snippet module --}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import System.Console.Readline (readline)

-- Provided by the 'zlib' package on http://hackage.haskell.org/
import Codec.Compression.GZip (compress)

compressFile path = L.writeFile (path ++ ".gz") . compress

main = loop
 where
  loop = do
    maybeLine <- readline "Enter a file to compress> "
    case maybeLine of
      Nothing -> return ()      -- user entered EOF
      Just "" -> return ()      -- treat no name as "want to quit"
      Just name -> do
           flip catch print $ do
             content <- L.readFile name
             forkIO (compressFile name content)
             return ()
           loop
{-- /snippet module --}
