
import System.Environment
import Text.Printf
import Data.List

main = do
    [d] <- map read `fmap` getArgs
    printf "%f\n" (mean [1..d])

{-- snippet fold --}
data Pair = Pair !Int !Double

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    Pair n s       = foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n+1) (s+x)
{-- /snippet fold --}
