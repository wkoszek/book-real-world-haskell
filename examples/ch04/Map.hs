{-- snippet upperCase --}
import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []
{-- /snippet upperCase --}

{-- snippet square --}
square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square []     = []
{-- /snippet square --}

{-- snippet map2 --}
square2 xs = map squareOne xs
    where squareOne x = x * x

upperCase2 xs = map toUpper xs
{-- /snippet map2 --}

{-- snippet myMap --}
myMap :: (a -> b) -> [a] -> [b]

myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []
{-- /snippet myMap --}
