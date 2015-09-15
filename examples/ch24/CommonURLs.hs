{-- snippet countURLs --}
module Main where

import Control.Parallel.Strategies (NFData(..), rwhnf)
import Control.Monad (forM_)
import Data.List (foldl', sortBy)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import Text.Regex.PCRE.Light (compile, match)

import System.Environment (getArgs)
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)

countURLs :: [L.ByteString] -> M.Map S.ByteString Int
countURLs = mapReduce rwhnf (foldl' augment M.empty . L.lines)
                      rwhnf (M.unionsWith (+))
  where augment map line =
            case match (compile pattern []) (strict line) [] of
              Just (_:url:_) -> M.insertWith' (+) url 1 map
              _ -> map
        strict  = S.concat . L.toChunks
        pattern = S.pack "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"
{-- /snippet countURLs --}

{-- snippet main --}
instance NFData S.ByteString where
    rnf _ = ()    -- not built into Control.Parallel.Strategies

main = do
  args <- getArgs
  forM_ args $ \path -> do
    m <- chunkedReadWith countURLs path
    let mostPopular (_,a) (_,b) = compare b a
    mapM_ print . take 10 . sortBy mostPopular . M.toList $ m
{-- /snippet main --}
