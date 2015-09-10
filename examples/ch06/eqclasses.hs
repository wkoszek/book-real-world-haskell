{-- snippet color --}
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False
{-- /snippet color --}

stringEq :: [Char] -> [Char] -> Bool
-- Match if both are empty
stringEq [] [] = True
-- Evaluate when we have only one character in both
stringEq [x] [y] = x == y
-- If both start with the same char, check the rest
stringEq (x:xs) (y:ys) = 
    if x == y
        then stringEq xs ys
        else False
-- Everything else doesn't match
stringEq _ _ = False

{-- snippet basiceq --}
class BasicEq a where
    isEqual :: a -> a -> Bool
{-- /snippet basiceq --}

{-- snippet basicinstance --}
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False
{-- /snippet basicinstance--}

{-- snippet basiceq2 --}
class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
{-- /snippet basiceq2 --}

{-- snippet basiceq3 --}
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)
{-- /snippet basiceq3 --}

{-- snippet basiceq3inst --}
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False
{-- /snippet basiceq3inst --}

{-- snippet show --}
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"
{-- /snippet show --}
{-- snippet read --}
instance Read Color where
    -- readsPrec is the main function for parsing input
    readsPrec _ value = 
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (take (length attempt) value) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, drop (length attempt) value)]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs
{-- /snippet read --}
