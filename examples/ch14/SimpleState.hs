{-- snippet SimpleState --}
type SimpleState s a = s -> (a, s)
{-- /snippet SimpleState --}

{-- snippet StringState --}
type StringState a = SimpleState String a
{-- /snippet StringState --}
    
{-- snippet returnSt --}
returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)
{-- /snippet returnSt --}

{-- snippet returnAlt --}
returnAlt :: a -> SimpleState s a
returnAlt a s = (a, s)
{-- /snippet returnAlt --}

{-- snippet bindSt --}
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'
{-- /snippet bindSt --}

{-- snippet bindAlt.type --}
bindAlt :: (s -> (a, s))        -- step
        -> (a -> s -> (b, s))   -- makeStep
        -> (s -> (b, s))        -- (makeStep result) newState
{-- /snippet bindAlt.type --}

{-- snippet bindAlt --}
-- m == step
-- k == makeStep
-- s == oldState

bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    in  (makeStep result) newState
{-- /snippet bindAlt --}

{-- snippet getPut --}
getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
{-- /snippet getPut --}
