module Arbitrary2 where

import Prelude hiding (Bool(..))
import Test.QuickCheck

-- define our own
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

{-- snippet Instance2 --}
instance Arbitrary Ternary where
  arbitrary     = do
      n <- choose (0, 2) :: Gen Int
      return $ case n of
                    0 -> Yes
                    1 -> No
                    _ -> Unknown
{-- /snippet Instance2 --}

