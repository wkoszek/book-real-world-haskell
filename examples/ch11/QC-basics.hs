{-- snippet module --}
import Test.QuickCheck
import Data.List
{-- /snippet module --}

-- Simple model testing

{-- snippet mysort --}
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs
{-- /snippet mysort --}

{-- snippet idempotent --}
prop_idempotent xs = qsort (qsort xs) == qsort xs
{-- /snippet idempotent --}

{-- snippet relatives_wrong --}
prop_minimum xs         = head (qsort xs) == minimum xs
{-- /snippet relatives_wrong --}

{-- snippet relatives_right --}
prop_minimum' xs         = not (null xs) ==> head (qsort xs) == minimum xs
{-- /snippet relatives_right --}

{-- snippet relatives --}
prop_ordered xs = ordered (qsort xs)
    where ordered []       = True
          ordered [x]      = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs         =
    not (null xs) ==>
        last (qsort xs) == maximum xs

prop_append xs ys       =
    not (null xs) ==>
    not (null ys) ==>
        head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)
{-- /snippet relatives --}

{-- snippet model --}
prop_sort_model xs      = sort xs == qsort xs
{-- /snippet model --}

-- Generating random data
