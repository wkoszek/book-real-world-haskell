{-- snippet Foo --}
data Foo a = Foo a
           
instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
{-- /snippet Foo --}

{-- snippet Bar --}
data Eq a => Bar a = Bar a

instance Functor Bar where
    fmap f (Bar a) = Bar (f a)
{-- /snippet Bar --}
