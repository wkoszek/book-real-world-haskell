{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Pearson (main) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as U
import qualified Data.Map as M

newtype User = U Int
    deriving (Eq, Ord, Show)

newtype Item = I Int
    deriving (Eq, Ord, Show)

newtype Rating = R Int
    deriving (Eq, Ord, Show)

data Record = Record {
      recUser   :: !User
    , recItem   :: !Item
    , recRating :: !Rating
    } deriving (Eq, Ord, Show)

readInt s = case B.readInt s of
              Just a -> a
              _ -> error "eek"

readRatings :: FilePath -> IO [Record]
readRatings path = (map parseRating . B.lines) `fmap` B.readFile path

parseRating line = let (userId, a) = readInt line
                       (movieId, b) = readInt (U.unsafeDrop 2 a)
                       (rating, _) = readInt (U.unsafeDrop 2 b)
                   in  Record (U userId) (I movieId) (R rating)

(*) `on` f = \x y -> f x * f y

ratingMapWith :: (Ord a) => (Record -> a) -> [Record] -> M.Map a [Record]
ratingMapWith f recs = M.fromAscListWith (++) [(f r, [r]) | r <- recs]

main = do
  r <- readRatings "/home/bos/downloads/million-ml-data/ratings.dat"
  let m = ratingMapWith recUser r
  print (M.size m)
