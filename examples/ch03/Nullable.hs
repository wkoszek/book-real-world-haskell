import Prelude hiding (Maybe)

{-- snippet Nullable --}
data Maybe a = Just a
             | Nothing
{-- /snippet Nullable --}

{-- snippet wrappedTypes --}
someBool = Just True

someString = Just "something"
{-- /snippet wrappedTypes --}
                

{-- snippet parens --}
wrapped = Just (Just "wrapped")
{-- /snippet parens --}
