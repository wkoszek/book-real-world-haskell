
{-- snippet newForeignPtr --}
newForeignPtr :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
{-- /snippet newForeignPtr --}

{-- snippet useAsCString --}
useAsCString :: ByteString -> (CString -> IO a) -> IO a
{-- /snippet useAsCString --}

{-- snippet alloc --}
alloca :: Storable a => (Ptr a -> IO b) -> IO b
{-- /snippet alloc --}

{-- snippet unsafePerformIO --}
unsafePerformIO :: IO a -> a
{-- /snippet unsafePerformIO --}
