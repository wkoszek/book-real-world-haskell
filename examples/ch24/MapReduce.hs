module MapReduce
    (
      mapReduce
    , simpleMapReduce
    -- exported for convenience
    , rnf
    , rwhnf
    ) where

import Control.Parallel (pseq)
import Control.Parallel.Strategies

{-- snippet simpleMapReduce.type --}
simpleMapReduce
    :: (a -> b)      -- map function
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
{-- /snippet simpleMapReduce.type --}

{-- snippet simpleMapReduce --}
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc
{-- /snippet simpleMapReduce --}

{-- snippet mapReduce.type --}
mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
{-- /snippet mapReduce.type --}

{-- snippet mapReduce --}
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat
{-- /snippet mapReduce --}
