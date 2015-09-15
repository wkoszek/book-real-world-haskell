-- | An abstract pointer to a compiled PCRE Regex structure
-- The structure allocated by the PCRE library will be deallocated
-- automatically by the Haskell storage manager.
--


{-- snippet data --}
data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)
{-- /snippet data --}

{-- snippet unit --}
type PCRE = ()
{-- /snippet unit --}

{-- snippet pcre_compile --}
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)
{-- /snippet pcre_compile --}

{-- snippet compiletype --}
compile :: ByteString -> [PCREOption] -> Either String Regex
{-- /snippet compiletype --}

{-- snippet compileReal --}
compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pattern -> do
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

