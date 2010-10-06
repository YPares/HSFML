{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.VideoMode where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.VideoMode


foreign import ccall unsafe "wrp_sfVideoMode_GetDesktopMode"
  c_VideoMode_GetDesktopMode :: Ptr (CUInt, CUInt, CUInt) -> IO ()

foreign import ccall unsafe "wrp_sfVideoMode_GetMode"
  c_VideoMode_GetMode :: CSize -> Ptr (CUInt, CUInt, CUInt) -> IO ()

foreign import ccall unsafe "sfVideoMode_GetModesCount"
  c_VideoMode_GetModesCount :: IO CSize

foreign import ccall unsafe "wrp_sfVideoMode_IsValid"
  c_VideoMode_IsValid :: Ptr (CUInt, CUInt, CUInt) -> IO Bool

