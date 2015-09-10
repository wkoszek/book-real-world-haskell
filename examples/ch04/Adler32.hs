import Control.Arrow ((***))
import Control.Monad (join)
{-- snippet adler32 --}
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base = 65521

adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _     = (b `shiftL` 16) .|. a
{-- /snippet adler32 --}

{-- snippet adler32_try2 --}
adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff)) `mod` base
                  b' = (a' + b) `mod` base
              in helper (a',b') xs
          helper (a,b) _     = (b `shiftL` 16) .|. a
{-- /snippet adler32_try2 --}

{-- snippet adler32_foldl --}
adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)
{-- /snippet adler32_foldl --}

adler32_golf = uncurry (flip ((.|.) . (`shiftL` 16))) . foldl f (1,0)
  where f (a,b) x = join (***) ((`mod` base) . (a + (ord x .&. 0xff) +)) (0,b)
