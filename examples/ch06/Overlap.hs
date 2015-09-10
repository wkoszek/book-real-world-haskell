{-# LANGUAGE FlexibleInstances #-}

{-- snippet Borked --}
class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance Borked (Int, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
    bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"
{-- /snippet Borked --}

{-- snippet wimply --}
instance Borked a => Borked (Int, a) where
    bork (a, b) = bork a ++ ", " ++ bork b

instance Borked a => Borked (a, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b
{-- /snippet wimply --}
