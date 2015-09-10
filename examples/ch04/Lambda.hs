{-- snippet safeHead --}
safeHead (x:_) = Just x
safeHead _ = Nothing
{-- /snippet safeHead --}

{-- snippet unsafeHead --}
unsafeHead = \(x:_) -> x
{-- /snippet unsafeHead --}
