{-- snippet testme --}
{-# LANGUAGE PatternGuards #-}

testme x xs | Just y <- lookup x xs, y > 3 = y
            | otherwise                    = 0
{-- /snippet testme --}

{-- snippet testme_noguards --}
testme_noguards x xs = case lookup x xs of
                         Just y | y > 3 -> y
                         _              -> 0
{-- /snippet testme_noguards --}
