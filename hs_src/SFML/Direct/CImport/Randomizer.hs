{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Randomizer where

import Foreign.C.Types


foreign import ccall unsafe "sfRandom_SetSeed"
  c_Random_SetSeed :: CUInt -> IO ()

foreign import ccall unsafe "sfRandom_GetSeed"
  c_Random_GetSeed :: IO CUInt

foreign import ccall unsafe "sfRandom_Float"
  c_Random_Float :: CFloat -> CFloat -> IO CFloat

foreign import ccall unsafe "sfRandom_Int"
  c_Random_Int :: CInt -> CInt -> IO CInt

