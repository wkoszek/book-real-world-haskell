
module T where

import Foreign

{-- snippet nullary --}
newtype PCRE = PCRE (Ptr PCRE)
{-- /snippet nullary --}
