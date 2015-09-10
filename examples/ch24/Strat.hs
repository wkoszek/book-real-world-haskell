import Control.Parallel (par)
import Control.Parallel.Strategies (parZipWith)

{-- snippet Strategy --}
type Done = ()

type Strategy a = a -> Done
{-- /snippet Strategy --}

{-- snippet r0 --}
r0 :: Strategy a 
r0 _ = ()
{-- /snippet r0 --}

{-- snippet rwhnf --}
rwhnf :: Strategy a 
rwhnf x = x `seq` ()  
{-- /snippet rwhnf --}

{-- snippet NFData --}
class NFData a where
  rnf :: Strategy a
  rnf = rwhnf
{-- /snippet NFData --}

{-- snippet instances --}
instance NFData Char
instance NFData Int

instance NFData a => NFData (Maybe a) where
    rnf Nothing  = ()
    rnf (Just x) = rnf x

{- ... and so on ... -}
{-- /snippet instances --}

{-- snippet seqList --}
seqList :: Strategy a -> Strategy [a]
seqList strat []     = ()
seqList strat (x:xs) = strat x `seq` (seqList strat xs)
{-- /snippet seqList --}

{-- snippet parList --}
parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)
{-- /snippet parList --}

{-- snippet parMap --}
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat
{-- /snippet parMap --}

{-- snippet using --}
using :: a -> Strategy a -> a
using x s = s x `seq` x
{-- /snippet using --}

{-- snippet vectorSum --}
vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
vectorSum' = parZipWith rnf (+)
{-- /snippet vectorSum --}
