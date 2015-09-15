{-- snippet pragma --}
{-# LANGUAGE ForeignFunctionInterface #-}
{-- /snippet pragma --}

{-- snippet imports --}
import Foreign
import Foreign.C.Types
{-- /snippet imports --}

{-- snippet binding --}
foreign import ccall "math.h sin"
     c_sin :: CDouble -> CDouble
{-- /snippet binding --}

{-- snippet highlevel --}
fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))
{-- /snippet highlevel --}

{-- snippet use --}
main = mapM_ (print . fastsin) [0/10, 1/10 .. 10/10]
{-- /snippet use --}
