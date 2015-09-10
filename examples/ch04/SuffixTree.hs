import Data.List

type Edge = (String, STree)

data STree = Node [Edge]
           | Leaf
             deriving (Show)

type EdgeFunction = [String] -> (Int, [String])

construct :: String -> STree
construct = suf . suffixes
    where suf [[]] = Leaf
          suf ss = Node [([a], suf n)
                         | a <- ['\0'..'\255'],
                           n@(sa:_) <- [ss `clusterBy` a]]
          clusterBy ss a = [cs | c:cs <- ss, c == a]

construct2 :: EdgeFunction -> [Char] -> String -> STree
construct2 edge alphabet = suf . suffixes
    where suf [[]] = Leaf
          suf ss = Node [(take (cpl+1) (a:sa), suf ssr)
                         | a <- alphabet,
                           n@(sa:_) <- [ss `clusterBy` a],
                           (cpl,ssr) <- [edge n]]
          clusterBy ss a = [cs | c:cs <- ss, c == a]

simple :: EdgeFunction
simple n = (0, n)

cst :: EdgeFunction
cst [s] = (length s, [[]])
cst awss@((a:w):ss)
    | null [c | c:_ <- ss, a /= c] = (cpl + 1, rss)
    | otherwise = (0, awss)
    where (cpl, rss) = cst (w:[u | _:u <- ss])

{-- snippet suffixes --}
suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []
{-- /snippet suffixes --}

{-- snippet noAsPattern --}
noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []                 
{-- /snippet noAsPattern --}

{-- snippet suffixes2 --}
suffixes2 xs = init (tails xs)
{-- /snippet suffixes2 --}

{-- snippet compose --}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
{-- /snippet compose --}

{-- snippet suffixes3 --}
suffixes3 xs = compose init tails xs
{-- /snippet suffixes3 --}

{-- snippet suffixes4 --}
suffixes4 = compose init tails
{-- /snippet suffixes4 --}

{-- snippet suffixes5 --}
suffixes5 = init . tails
{-- /snippet suffixes5 --}
