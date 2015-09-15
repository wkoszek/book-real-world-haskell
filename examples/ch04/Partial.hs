import Data.List

{-- snippet isInAny --}
isInAny needle haystack = any inSequence haystack
    where inSequence s = needle `isInfixOf` s
{-- /snippet isInAny --}

{-- snippet isInAny2 --}
isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
{-- /snippet isInAny2 --}

{-- snippet isInAny3 --}
isInAny3 needle haystack = any (isInfixOf needle) haystack
{-- /snippet isInAny3 --}

{-- snippet isInAny4 --}
isInAny4 needle haystack = any (needle `isInfixOf`) haystack
{-- /snippet isInAny4 --}
