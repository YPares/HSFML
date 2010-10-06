{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Font where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import SFML.Direct.Types.Font
import SFML.Direct.Types.SfInts


foreign import ccall unsafe "sfFont_Create"
  c_Font_Create :: IO (Ptr AbsFont)

foreign import ccall unsafe "sfFont_CreateFromFile"
  c_Font_CreateFromFile :: CString -> CUInt -> Ptr SfUint32 -> IO (Ptr AbsFont)

foreign import ccall unsafe "sfFont_CreateFromMemory"
  c_Font_CreateFromMemory :: CString -> CSize -> CUInt -> Ptr SfUint32 -> IO (Ptr AbsFont)

foreign import ccall unsafe "sfFont_GetCharacterSize"
  c_Font_GetCharacterSize :: Ptr AbsFont -> IO CUInt

foreign import ccall unsafe "sfFont_GetDefaultFont"
  c_Font_GetDefaultFont :: IO (Ptr AbsFont)

