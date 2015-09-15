{-- snippet where --}
foo = x
    where x = y
              where y = 2
{-- /snippet where --}

{-- snippet let --}
bar = let b = 2
          c = True
      in let a = b
         in (a, c)
{-- /snippet let --}
