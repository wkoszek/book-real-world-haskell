{-- snippet headers --}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>
{-- /snippet headers --}

{-- snippet newtype --}
-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)
{-- /snippet newtype --}

{-- snippet constants --}
-- PCRE compile options
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  }
{-- /snippet constants --}

{-- snippet combine --}
-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
{-- /snippet combine --}
