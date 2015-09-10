-- Idea courtesy of William Lee Irwin.

{-- snippet type --}
import Data.Char (digitToInt) -- we'll need ord shortly

asInt :: String -> Int
{-- /snippet type --}

{-- snippet loop --}
loop :: Int -> String -> Int

asInt xs = loop 0 xs
{-- /snippet loop --}

{-- snippet base --}
loop acc [] = acc
{-- /snippet base --}

{-- snippet inductive --}
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
{-- /snippet inductive --}
