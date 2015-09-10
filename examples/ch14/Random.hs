module Random where

import Control.Monad (liftM2)
import Control.Monad.State

{-- snippet rand --}
import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))
{-- /snippet rand --}

{-- snippet twoBadRandoms --}
twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
{-- /snippet twoBadRandoms --}

{-- snippet twoGoodRandoms --}
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')
{-- /snippet twoGoodRandoms --}

{-- snippet RandomState --}
type RandomState a = State StdGen a
{-- /snippet RandomState --}

{-- snippet getRandom --}
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val
{-- /snippet getRandom --}

{-- snippet getRandomDo --}
getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val
{-- /snippet getRandomDo --}

getTwoRandomsUgly :: Random a => RandomState (a, a)
getTwoRandomsUgly = do
  a <- getRandom
  b <- getRandom
  return (a, b)

{-- snippet getTwoRandoms --}
getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom
{-- /snippet getTwoRandoms --}

{-- snippet runTwoRandoms --}
runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result
{-- /snippet runTwoRandoms --}

{-- snippet CountedRandom --}
data CountedRandom = CountedRandom {
      crGen :: StdGen
    , crCount :: Int
    }

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val
{-- /snippet CountedRandom --}

{-- snippet getCount --}
getCount :: CRState Int
getCount = crCount `liftM` get
{-- /snippet getCount --}

{-- snippet putCount --}
putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }
{-- /snippet putCount --}

{-- snippet putCountModify --}
putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st { crCount = a }
{-- /snippet putCountModify --}
