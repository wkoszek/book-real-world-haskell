{-- snippet foo --}
foo = let a = 1
      in let b = 2
         in a + b
{-- /snippet foo --}

{-- snippet bar --}
bar = let x = 1
      in ((let x = "foo" in x), x)
{-- /snippet bar --}

{-- snippet quux --}
quux a = let a = "foo"
         in a ++ "eek!"
{-- /snippet quux --}
