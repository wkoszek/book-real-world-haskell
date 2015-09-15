module GroupLens where

import Control.Parallel.Strategies
import Data.Function (on)
import Data.List (groupBy)
import qualified Data.Map as M
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B
import qualified SlopeTwo as S

readRating :: B.ByteString -> (Int, Int, Int)
readRating line = maybe (error "impossible") id $ do
                    (userId, a) <- B.readInt line
                    (movieId, b) <- B.readInt (B.drop 2 a)
                    (rating, _) <- B.readInt (B.drop 2 b)
                    return (userId, movieId, rating)

newtype Fap k a = Fap {unfap :: M.Map k a}

instance NFData (Fap k a) where
    rnf (Fap m) = rnf (M.size m)

main = do
  (ratingFile:userId:movieIds) <- getArgs
  contents <- B.readFile ratingFile
  let rating = {-# SCC "rating" #-} groupBy compareUser . map readRating . B.lines $ contents
      compareUser (a,_,_) (b,_,_) = a == b
      asRating (_,b,c) = (b, fromIntegral c)
      foo = S.update S.empty . parMap rwhnf ( {-# SCC "fl" #-} M.fromList . map asRating) $ rating
  print (S.size foo)
