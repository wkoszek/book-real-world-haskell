{-- snippet whichFruit --}
data Fruit = Apple | Orange

apple = "apple"

orange = "orange"        

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple  -> Apple
                 orange -> Orange
{-- /snippet whichFruit --}

{-- snippet equational --}
equational apple = Apple
equational orange = Orange
{-- /snippet equational --}

{-- snippet betterFruit --}
betterFruit f = case f of
                  "apple"  -> Apple
                  "orange" -> Orange
{-- /snippet betterFruit --}
                              
