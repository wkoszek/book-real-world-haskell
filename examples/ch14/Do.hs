act = undefined
act1 = undefined
act2 = undefined
actN = undefined

expr1 = undefined
expr2 = undefined
exprN = undefined

doNotation1, doNotation2, doNotation3, doNotation4 :: Maybe ()
translated1, translated2, translated3, translated4 :: Maybe ()
finalTranslation2, semicolon, semicolonTranslated :: Maybe ()

{-- snippet doNotation1 --}
doNotation1 =
    do act
{-- /snippet doNotation1 --}

{-- snippet translated1 --}
translated1 =
    act
{-- /snippet translated1 --}

{-- snippet doNotation2 --}
doNotation2 =
    do act1
       act2
       {- ... etc. -}
       actN
{-- /snippet doNotation2 --}

{-- snippet translated2 --}
translated2 =
    act1 >>
    do act2
       {- ... etc. -}
       actN

finalTranslation2 =
    act1 >>
    act2 >>
    {- ... etc. -}
    actN
{-- /snippet translated2 --}
                 
{-- snippet doNotation3 --}
doNotation3 =
    do pattern <- act1
       act2
       {- ... etc. -}
       actN
{-- /snippet doNotation3 --}

{-- snippet translated3 --}
translated3 =
    let f pattern = do act2
                       {- ... etc. -}
                       actN
        f _     = fail "..."
    in act1 >>= f
{-- /snippet translated3 --}

{-- snippet doNotation4 --}
doNotation4 =
    do let val1 = expr1
           val2 = expr2
           {- ... etc. -}
           valN = exprN
       act1
       act2
       {- ... etc. -}
       actN
{-- /snippet doNotation4 --}

{-- snippet translated4 --}
translated4 =
    let val1 = expr1
        val2 = expr2
        valN = exprN
    in do act1
          act2
          {- ... etc. -}
          actN
{-- /snippet translated4 --}

{-- snippet robust --}
robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x
{-- /snippet robust --}

{-- snippet semicolon --}
semicolon = do
  {
    act1;
    val1 <- act2;
    let { val2 = expr1 };
    actN;
  }
{-- /snippet semicolon --}

{-- snippet semicolonTranslated --}
semicolonTranslated =
    act1 >>
    let f val1 = let val2 = expr1
                 in actN
        f _ = fail "..."
    in act2 >>= f
{-- /snippet semicolonTranslated --}
