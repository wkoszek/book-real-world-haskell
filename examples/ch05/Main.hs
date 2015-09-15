{-- snippet module --}
module Main () where

import SimpleJSON

main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
{-- /snippet module --}
