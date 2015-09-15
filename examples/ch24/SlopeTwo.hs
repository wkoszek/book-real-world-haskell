{-# LANGUAGE BangPatterns, FlexibleInstances #-}

module SlopeTwo
    (
      SlopeOne
    , Count
    , RatingValue
    , Rating
    , empty
    , size
    , update
    , predict
    ) where

import Control.Parallel.Strategies
import qualified Data.Map as M
import Debug.Trace


type Count = Int
type RatingValue = Double

-- The Rating is the known (item,Rating) information for a particular "user"
type Rating item = M.Map item RatingValue

-- The SlopeOne matrix is indexed by pairs of items and is implemented
-- as a sparse map of distinct ascending lists.  The 'update' and
-- 'predict' functions do not need the inner type to actually be a
-- map, so the list saves space and complexity.
newtype SlopeOne item = SlopeOne (M.Map item [Tup item])
  deriving (Eq, Show)
 
-- Strict triple tuple type for SlopeOne internals
data Tup item = Tup {
      itemT :: !item
    , countT :: !Count
    , ratingT :: !RatingValue
    } deriving (Eq, Show)
 
empty :: SlopeOne item
empty = SlopeOne M.empty
 
size :: SlopeOne item -> Int
size (SlopeOne m) = M.size m

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (y,ys) = splitAt n xs
                in y:chunksOf n ys

instance NFData (Tup item)

unionsWith :: (Ord k, NFData k, NFData a) => (a->a->a) -> [M.Map k a] -> M.Map k a
unionsWith f ms = M.unionsWith f . parMap rwhnf (M.unionsWith f) $
                  trace ("l " ++ show (length ms)) $ chunksOf 100 ms

update :: (NFData item, Ord item) => SlopeOne item -> [Rating item] -> SlopeOne item
update s@(SlopeOne matrixIn) usersRatingsIn | null usersRatings = s
                                            | otherwise =
    SlopeOne . unionsWith mergeAdd . (matrixIn:) . parMap rwhnf fromRating $ usersRatings
  where usersRatings = filter ((1<) . M.size) usersRatingsIn
        -- fromRating converts a Rating into a Map of Lists, a singleton SlopeOne.
        fromRating userRatings = M.mapWithKey expand userRatings
          where expand item1 rating1 = map makeTup . M.toAscList . M.delete item1 $ userRatings
                  where makeTup (item2,rating2) = Tup item2 1 (rating1-rating2)
 
-- 'mergeAdd' is a helper for 'update'.
-- Optimized traversal of distinct ascending lists to perform additive merge.
mergeAdd :: Ord item => [Tup item] -> [Tup item] -> [Tup item]
mergeAdd xa@(x:xs) ya@(y:ys) =
  case compare (itemT x) (itemT y) of
    LT -> x : mergeAdd xs ya
    GT -> y : mergeAdd xa ys
    EQ -> Tup (itemT x) (countT x + countT y) (ratingT x + ratingT y) : mergeAdd xs ys
mergeAdd xs [] = xs
mergeAdd [] ys = ys
 
-- The output Rating has no items in common with the input Rating and
-- only includes positively weighted ratings.
predict :: Ord item => SlopeOne item -> Rating item -> Rating item
predict (SlopeOne matrixIn) userRatings =
    M.mapMaybe (computeRating ratingList) (M.difference matrixIn userRatings)
  where ratingList = M.toAscList userRatings
 
-- 'computeRating' is a helper for 'predict'.
-- Optimized traversal of distinct ascending lists to compute positive weighted rating.
computeRating :: (Ord item) => [(item,RatingValue)] -> [Tup item] -> Maybe RatingValue
computeRating !xa@(x:xs) !ya@(y:ys) =
  case compare (fst x) (itemT y) of
    LT -> computeRating xs ya
    GT -> computeRating xa ys
    EQ -> helper (countT y) (ratingT y + fromIntegral (countT y) * snd x) xs ys
 where
  helper :: (Ord item) => Count -> RatingValue -> [(item,RatingValue)] -> [Tup item] -> Maybe RatingValue
  helper !count !rating !xa@(x:xs) !ya@(y:ys) =
    case compare (fst x) (itemT y) of
      LT -> helper count rating xs ya
      GT -> helper count rating xa ys
      EQ -> helper (count + countT y) (rating + ratingT y + fromIntegral (countT y) * (snd x)) xs ys
  helper !count !rating _ _  | rating > 0 = Just (rating / fromIntegral count)
                             | otherwise = Nothing
computeRating _ _ = Nothing
