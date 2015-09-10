{-- snippet headers --}
{-# INCLUDE <pcre.h> #-}
{-# LINE 1 "Regex_hsc_const.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex_hsc_const.hsc" #-}

module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex_hsc_const.hsc" #-}
{-- /snippet headers --}

{-- snippet newtype --}
-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Ord,Show,Read)
{-- /snippet newtype --}

{-- snippet generatedconsts --}
caseless       :: PCREOption
caseless       = PCREOption 1
{-# LINE 21 "Regex.hsc" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 24 "Regex.hsc" #-}

dotall         :: PCREOption
dotall         = PCREOption 4
{-# LINE 27 "Regex.hsc" #-}
{-- /snippet generatedconsts --}
