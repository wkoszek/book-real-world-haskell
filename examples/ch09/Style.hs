{-- snippet goodLet --}
tidyLet = let foo = undefined
              bar = foo * 2
          in undefined
{-- /snippet goodLet --}

{-- snippet badLet --}
weirdLet = let foo = undefined
               bar = foo * 2
    in undefined

strangeLet = let foo = undefined
                 bar = foo * 2 in
    undefined
{-- /snippet badLet --}

{-- snippet punctuation --}
unusualPunctuation =
    [ (x,y) | x <- [1..a], y <- [1..b] ] where {
                                           b = 7;
 a = 6 }

preferredLayout = [ (x,y) | x <- [1..a], y <- [1..b] ]
    where b = 7
          a = 6
{-- /snippet punctuation --}

commonDo, rareDo :: Monad m => m ()

{-- snippet do --}
commonDo = do
  something <- undefined
  return ()

-- not seen very often
rareDo =
  do something <- undefined
     return ()
{-- /snippet do --}

{-- snippet indent --}
normalIndent =
    undefined

strangeIndent =
                           undefined
{-- /snippet indent --}

{-- snippet where --}
goodWhere = take 5 lambdas
    where lambdas = []

alsoGood =
    take 5 lambdas
  where
    lambdas = []

badWhere =           -- legal, but ugly and hard to read
    take 5 lambdas
    where
    lambdas = []
{-- /snippet where --}
