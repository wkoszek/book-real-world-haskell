{-- snippet functor --}
fmap id        ==   id 
fmap (f . g)   ==   fmap f . fmap g
{-- /snippet functor --}

{-- snippet leftIdentity --}
return x >>= f            ===   f x
{-- /snippet leftIdentity --}

{-- snippet leftIdentityDo --}
do y <- return x
   f y                    ===   f x
{-- /snippet leftIdentityDo --}

{-- snippet rightIdentity --}
m >>= return              ===   m 
{-- /snippet rightIdentity --}

{-- snippet rightIdentityDo --}
do y <- m
   return y               ===   m
{-- /snippet rightIdentityDo --}

{-- snippet associativity --}
m >>= (\x -> f x >>= g)   ===   (m >>= f) >>= g
{-- /snippet associativity --}

{-- snippet associativityLeft --}
m >>= s
  where s x = f x >>= g
{-- /snippet associativityLeft --}

{-- snippet associativityRight --}
t >>= g
  where t = m >>= f
{-- /snippet associativityRight --}

{-- snippet associativityRewrite --}
m >>= s                   ===   t >>= g
{-- /snippet associativityRewrite --}
