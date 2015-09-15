{-# INCLUDE <pcre.h> #-}
{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex.hsc" #-}

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Ord,Show,Read)

-- PCRE compile options
{-- snippet result --}
caseless              :: PCREOption
caseless              = PCREOption 1
dollar_endonly        :: PCREOption
dollar_endonly        = PCREOption 32
dotall                :: PCREOption
dotall                = PCREOption 4
{-- /snippet result --}

dupnames              :: PCREOption
dupnames              = PCREOption 524288
extended              :: PCREOption
extended              = PCREOption 8
extra                 :: PCREOption
extra                 = PCREOption 64
firstline             :: PCREOption
firstline             = PCREOption 262144
multiline             :: PCREOption
multiline             = PCREOption 2
newline_cr            :: PCREOption
newline_cr            = PCREOption 1048576
newline_crlf          :: PCREOption
newline_crlf          = PCREOption 3145728
newline_lf            :: PCREOption
newline_lf            = PCREOption 2097152
no_auto_capture       :: PCREOption
no_auto_capture       = PCREOption 4096
ungreedy              :: PCREOption
ungreedy              = PCREOption 512

{-# LINE 32 "Regex.hsc" #-}

{-- snippet bitwise --}
-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
{-- /snippet bitwise --}
