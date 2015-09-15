{-- snippet braces --}
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1;  b = 2;
        c = 3 }
      in a + b + c
{-- /snippet braces --}
