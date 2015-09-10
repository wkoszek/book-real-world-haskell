{-- snippet newtype --}
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)
{-- /snippet newtype --}

{-- snippet UniqueID --}
newtype UniqueID = UniqueID Int
    deriving (Eq)
{-- /snippet UniqueID --}
