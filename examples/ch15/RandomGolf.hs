import Supply
import System.Random
{-- snippet randomsIO_golfed --}
import Control.Arrow (first)

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)
{-- /snippet randomsIO_golfed --}
