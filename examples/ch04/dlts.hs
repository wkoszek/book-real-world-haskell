{-- snippet dlts --}
import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
{-- /snippet dlts --}
{-- snippet step --}
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise                     = ds
        secondWord = head . tail . words
{-- /snippet step --}
