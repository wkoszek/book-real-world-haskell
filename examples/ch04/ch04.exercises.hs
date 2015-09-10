import Data.Char (ord, digitToInt)
import Prelude hiding (concat, takeWhile)

{-- snippet concat --}
concat :: [[a]] -> [a]
{-- /snippet concat --}
concat = foldr (++) []

{-- snippet takeWhile --}
takeWhile :: (a -> Bool) -> [a] -> [a]
{-- /snippet takeWhile --}
takeWhile p = foldr step []
    where step x xs | p x       = x:xs
                    | otherwise = []

{-- snippet groupBy --}
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
{-- /snippet groupBy --}
groupBy f = foldr step []
    where step x [] = [[x]]
          step x ((y:ys):zs) | f x y     = (x:y:ys):zs
                             | otherwise = [x]:(y:ys):zs

-- Idea courtesy of William Lee Irwin.
{-- snippet asInt_fold --}
asInt_fold :: String -> Int
{-- /snippet asInt_fold --}
asInt_fold ('-':xs) = negate (asInt_fold xs)
asInt_fold xs = foldl (\a b -> (a*10) + (digitToInt b)) 0 xs

asInt_fold' [] = error "empty string"
asInt_fold' xs = foldr step 0 xs
    where step c n
              | c `elem` ['0'..'9'] = let n' = n * 10 + ord c - zero
                                      in if n' < n
                                         then error "numeric overflow"
                                         else n'
              | otherwise = error ("non-digit " ++ show c)
          zero = ord '0'

{-- snippet asInt_either --}
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
{-- /snippet asInt_either --}
asInt_either ('-':xs) = case asInt_either' xs of
                          Left err -> Left err
                          Right val -> Right (negate val)
asInt_either xs = asInt_either' xs

asInt_either' [] = Left "empty string"
asInt_either' xs = foldr step (Right 0) xs
    where step c (Right n)
              | c `elem` ['0'..'9'] = let n' = n * 10 + ord c - zero
                                      in if n' < n
                                         then Left "numeric overflow"
                                         else Right n'
              | otherwise = Left ("non-digit " ++ show c)
          step _ err = err
          zero = ord '0'

breakList :: ([a] -> Maybe [a]) -> [a] -> ([a], [a], [a])
breakList p = helper []
    where helper acc xs@(x:xs') = case p xs of
                           Just sep -> (reverse acc, sep,
                                        foldr (const tail) xs sep)
                           Nothing -> helper (x:acc) xs'
          helper acc [] = (reverse acc, [], [])

{-- snippet safe --}
safeHead :: [a] -> Maybe a
safeTail :: [a] -> Maybe [a]
safeLast :: [a] -> Maybe a
safeInit :: [a] -> Maybe [a]
{-- /snippet safe --}

safeHead (x:_) = Just x
safeHead _     = Nothing

safeTail (_:xs) = Just xs
safeTail _      = Nothing

safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs
safeLast []     = Nothing

safeInit []     = Nothing
safeInit [x]    = Just []
safeInit (x:xs) = maybe Nothing (Just . (x:)) (safeInit xs)

{-- snippet splitWith --}
splitWith :: (a -> Bool) -> [a] -> [[a]]
{-- /snippet splitWith --}
splitWith _ [] = []
splitWith p xs = let (y, ys) = break p xs
                 in y : splitWith p (dropWhile p ys)
                   
