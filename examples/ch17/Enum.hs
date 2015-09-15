{-# LANGUAGE CPP #-}

#include <pcre.h>

import Foreign
import Foreign.C.Types

pcreCaseless :: CInt
pcreCaseless = PCRE_CASELESS

main = print pcreCaseless
