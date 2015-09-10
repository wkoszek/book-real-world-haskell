import Prelude hiding (filter, foldl, foldr)

{-- snippet foldl --}
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero
{-- /snippet foldl --}

{-- snippet foldr --}
foldr :: (a -> b -> b) -> b -> [a] -> b

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _    zero []     = zero
{-- /snippet foldr --}

{-- snippet myMap --}
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
    where step x ys = f x : ys
{-- /snippet myMap --}

{-- snippet myFoldl --}
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
{-- /snippet myFoldl --}

{-- snippet filter --}
filter :: (a -> Bool) -> [a] -> [a]
filter p []   = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
{-- /snippet filter --}

{-- snippet myFilter --}
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys
{-- /snippet myFilter --}

{-- snippet identity --}
identity :: [a] -> [a]
identity xs = foldr (:) [] xs
{-- /snippet identity --}

{-- snippet append --}
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
{-- /snippet append --}
              
{-
{-- snippet foldl.expand --}
foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)
{-- /snippet foldl.expand --}
-}

{-
{-- snippet foldr.expand --}
foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))
{-- /snippet foldr.expand --}
-}

{-
{-- snippet foldr.sub --}
1 : (2 : (3 : []))
1 + (2 + (3 + 0 ))
{-- /snippet foldr.sub --}
-}

{-- snippet strict --}
foldl' _    zero []     = zero
foldl' step zero (x:xs) =
    let new = step zero x
    in  new `seq` foldl' step new xs
{-- /snippet strict --}

{-
{-- snippet step1 --}
foldl' (+) 1 (2:[])
{-- /snippet step1 --}

{-- snippet step2 --}
let new = 1 + 2
in new `seq` foldl' (+) new []
{-- /snippet step2 --}

{-- snippet step3 --}
foldl' (+) 3 []
{-- /snippet step3 --}

{-- snippet step4 --}
3
{-- /snippet step4 --}
-}
 
{-- snippet meaningless --}
tryToForce x = x `seq` x
{-- /snippet meaningless --}

{-- snippet hiddenInside --}
-- incorrect: seq is hidden by the application of someFunc
-- since someFunc will be evaluated first, seq may occur too late
hiddenInside x y = someFunc (x `seq` y)

-- incorrect: a variation of the above mistake
hiddenByLet x y z = let a = x `seq` someFunc y
                    in anotherFunc a z

-- correct: seq will be evaluated first, forcing evaluation of x
onTheOutside x y = x `seq` someFunc y
{-- /snippet hiddenInside --}

{-- snippet chained --}
chained x y z = x `seq` y `seq` someFunc z
{-- /snippet chained --}

{-- snippet badExpression --}
badExpression step zero (x:xs) =
    seq (step zero x)
        (badExpression step (step zero x) xs)
{-- /snippet badExpression --}

someFunc = error ""
anotherFunc = error ""

{-- snippet strictPair --}
strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList []     = []
{-- /snippet strictPair --}
