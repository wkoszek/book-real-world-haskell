{-- snippet all --}
insert :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insert key value [] = [(key, value)]
insert key value ((listkey, listvalue):remainder) 
    | key == listkey = (key, value) : remainder
    | otherwise = (listkey, listvalue) : insert key value remainder
{-- /snippet all --}
