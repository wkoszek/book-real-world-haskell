{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module RegexFull where

-- Foreigns
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Unsafe   as S
import qualified Data.ByteString.Internal as S

#include <pcre.h>

data Regex = Regex {-# UNPACK #-} !(ForeignPtr PCRE)
                   {-# UNPACK #-} !S.ByteString
        deriving (Eq, Ord, Show)

type PCRE = ()

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
#if __GLASGOW_HASKELL__
    deriving (Eq,Ord,Show,Read)
#endif

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

caseless       :: PCREOption
caseless       = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall         :: PCREOption
dotall         = PCREOption #const PCRE_DOTALL

{-- snippet pcre_compile --}
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)
{-- /snippet pcre_compile --}

{-- snippet compileReal --}
compile :: S.ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  S.useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions flags) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))
{-- /snippet compileReal --}


type PCREExtra = ()
type PCREExtraFlags = CInt
type PCREInfo = CInt

info_capturecount    = #const PCRE_INFO_CAPTURECOUNT

type PCREExecOption = CInt

foreign import ccall unsafe "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt

-- | Return information about a compiled pattern
foreign import ccall unsafe "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr ()
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt


match :: Regex -> S.ByteString -> Maybe [S.ByteString]
match (Regex pcre_fp _) subject = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr

    -- The smallest  size  for ovector that will allow for n captured
    -- substrings, in addition to the offsets  of  the  substring
    -- matched by the whole pattern, is (n+1)*3. (man pcreapi)

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)

    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = S.toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off) -- may contain binary zero bytes.
                         (fromIntegral len)
                         0
                         0
                         ovec
                         (fromIntegral ovec_size)

            if r < 0 -- errors, or error_no_match
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec $! o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    s `seq` loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

  where
    substring :: CInt -> CInt -> S.ByteString -> S.ByteString
    substring x y _ | x == y = S.empty -- XXX an unset subpattern
    substring a b s = end -- note that we're not checking...
        where
            start = S.unsafeDrop (fromIntegral a) s
            end   = S.unsafeTake (fromIntegral (b-a)) start

    capturedCount :: Ptr PCRE -> IO Int
    capturedCount regex_ptr =
        alloca $ \n_ptr -> do
             c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
             return . fromIntegral =<< peek (n_ptr :: Ptr CInt)
