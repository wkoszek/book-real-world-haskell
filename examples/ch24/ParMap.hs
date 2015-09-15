{-- snippet parallelMap --}
import Control.Parallel (par)

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x:xs) = let r = f x
                       in r `par` r : parallelMap f xs
parallelMap _ _      = []
{-- /snippet parallelMap --}

{-- snippet forceList --}
forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _      = ()
{-- /snippet forceList --}

{-- snippet stricterMap --}
stricterMap :: (a -> b) -> [a] -> [b]
stricterMap f xs = forceList xs `seq` map f xs
{-- /snippet stricterMap --}

{-- snippet forceListAndElts --}
forceListAndElts :: (a -> ()) -> [a] -> ()
forceListAndElts forceElt (x:xs) =
    forceElt x `seq` forceListAndElts forceElt xs
forceListAndElts _        _      = ()
{-- /snippet forceListAndElts --}
