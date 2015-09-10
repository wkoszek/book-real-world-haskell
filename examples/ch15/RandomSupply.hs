module RandomSupply (randomsIO) where

{-- snippet randomsIO --}
import Supply
import System.Random hiding (next)

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a, b) = split g
        in (randoms a, b)
{-- /snippet randomsIO --}
