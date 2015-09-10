module QC where

import Prettify2 -- needs to export constructors to be testable
import Data.Char
import Data.List
import Data.Word
import Control.Monad
import Data.Monoid
import Test.QuickCheck

{-- snippet ArbitraryChar --}
instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")
{-- /snippet ArbitraryChar --}

{-
    coarbitrary c = variant $ if n >= 0 then fromIntegral (2*n)
                                        else fromIntegral (2*(-n) + 1)
        where n = fromIntegral (ord c) :: Word8
-}


-- TODO, probabilities for Empty.

{-
{-- snippet ArbitraryDoc --}
instance Arbitrary Doc where
    arbitrary = do
        n <- choose (1,6) :: Gen Int
        case n of
             1 -> return Empty

             2 -> do x <- arbitrary
                     return (Char x)

             3 -> do x <- arbitrary
                     return (Text x)

             4 -> return Line

             5 -> do x <- arbitrary
                     y <- arbitrary
                     return (Concat x y)

             6 -> do x <- arbitrary
                     y <- arbitrary
                     return (Union x y)
{-- /snippet ArbitraryDoc --}
-}

{-- snippet ArbitraryDocConcise --}
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]
{-- /snippet ArbitraryDocConcise --}


-- nice properties

{-- snippet prop_empty --}
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x
{-- /snippet prop_empty --}

{-- snippet simple_stuff --}

prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == text (show d)

{-- /snippet simple_stuff --}

{-- snippet hcat --}

prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

{-- /snippet hcat --}

-- wrong: due to <> removing Empty's
{-- snippet punctuate_wrong --}

prop_punctuate s xs = punctuate s xs == intersperse s xs

{-- /snippet punctuate_wrong --}

{-- snippet punctuate --}
prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys
{-- /snippet punctuate --}

{-- snippet fsep --}

prop_fsep xs = fsep xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

{-- /snippet fsep --}

-- relating things to models:

prop_text_id x =
    pretty 100 (Text x) == x

prop_append x y =
    pretty n (Text x <> Text y) == x ++ y
  where
    n = 100

-- test monoid laws.
    --


{-- snippet monoid_prop --}
prop_mempty_id x =
    mempty `mappend` x == x
  &&
    x `mappend` mempty == (x :: Doc)

{-- /snippet monoid_prop --}

--
-- False, due to Union case.
--
prop_concat_append x y =
    pretty n (x <> y) == pretty n x ++ pretty n y
  where
    n = 100

{-

    the left of each Union is always the same width as, or wider than, the right.

-}

{-

Falsifiable, after 25 tests:
Concat (Char 'p') (Union Empty (Text "r"))
Union (Text "huM") (Text "H~mHrCX")

-}


{-
*Main Test.QuickCheck> quickCheck prop_concat_append
Falsifiable, after 15 tests:
Union (Text "") (Union (Concat (Text "") (Char 'o')) (Concat Empty Line))
Char 'v'
-}

{-
    *Main Test.QuickCheck> quickCheck prop_empty_id
    OK, passed 100 tests.
-}

{-
    Verbose check:

    Concat (Concat (Text "rfpqnotlwktrkxnivorky xc") (Char 'k')) (Union Empty (Text "vcozhnqeupgomkv zqugfygo"))
    88:
    Empty
    89:
    Text "hyyx"
    90:
    Line
    91:
    Char 'c'
    92:
    Text "ntgv s yytub"
    93:
    Char 'q'
    94:
    Text "kapxyqlnzzcdqupa"
    95:
    Concat (Union (Union Empty Line) (Char 'h')) (Char 'o')
    96:
    Concat (Concat (Char 'b') (Text "mavhza eybzgbrpnuryahr")) (Union (Text "aamwokb") (Text "ytt pqhf"))
    97:
    Char 'y'
    98:
    Char 'w'
    99:
    Text ""
    OK, passed 100 tests.

-}
