module Arbitrary where

{-- snippet Class --}
class Arbitrary a where
  arbitrary   :: Gen a
{-- /snippet Class --}

--  coarbitrary :: a -> Gen b -> Gen b

{-- snippet IntroductionForms --}
  elements :: [a] -> Gen a
  choose   :: Random a => (a, a) -> Gen a
  oneof    :: [Gen a] -> Gen a
{-- /snippet IntroductionForms --}

{-- snippet ternary --}
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)
{-- /snippet ternary --}

{-- snippet Instance --}
instance Arbitrary Ternary where
  arbitrary     = elements [Yes, No, Unknown]
{-- /snippet Instance --}

{-- snippet InstanceProduct --}
instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (x, y)
{-- /snippet InstanceProduct --}

{-- snippet InstanceRec --}
instance Arbitrary a => Arbitrary [a] where
  arbitrary =
      n <- choose (0, 1) :: Gen Int
      if n == 0
         then []
         else do x  <- arbitrary :: a
                 xs <- arbitrary :: Gen [a]
                 return (x : xs)
{-- /snippet InstanceRec --}

--  coarbitrary b = if b then variant 0 else variant 1
