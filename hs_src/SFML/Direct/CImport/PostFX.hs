{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.PostFX where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import SFML.Direct.Types.PostFX
import SFML.Direct.Types.Image


foreign import ccall unsafe "sfPostFX_CreateFromFile"
  c_PostFX_CreateFromFile :: CString -> IO (Ptr AbsPostFX)

foreign import ccall unsafe "sfPostFX_CreateFromMemory"
  c_PostFX_CreateFromMemory :: CString -> IO (Ptr AbsPostFX)

foreign import ccall unsafe "sfPostFX_SetParameter1"
  c_PostFX_SetParameter1 :: Ptr AbsPostFX -> CString -> CFloat -> IO ()

foreign import ccall unsafe "sfPostFX_SetParameter2"
  c_PostFX_SetParameter2 :: Ptr AbsPostFX -> CString -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfPostFX_SetParameter3"
  c_PostFX_SetParameter3 :: Ptr AbsPostFX -> CString -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfPostFX_SetParameter4"
  c_PostFX_SetParameter4 :: Ptr AbsPostFX -> CString -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfPostFX_SetTexture"
  c_PostFX_SetTexture :: Ptr AbsPostFX -> CString -> Ptr AbsImage -> IO ()

foreign import ccall unsafe "sfPostFX_CanUsePostFX"
  c_PostFX_CanUsePostFX :: IO Bool

