{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Image where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import SFML.Direct.Types.Image
import SFML.Direct.Types.Color
import SFML.Direct.Types.Rect
import SFML.Direct.Types.RenderWindow
import SFML.Direct.Types.SfInts


newtype PixelsPtr = PixelsPtr (Ptr CColorCoord)

foreign import ccall unsafe "sfImage_Create"
  c_Image_Create :: IO (Ptr AbsImage)

foreign import ccall unsafe "wrp_sfImage_CreateFromColor"
  c_Image_CreateFromColor :: CUInt -> CUInt -> Ptr CColor ->
                             IO (Ptr AbsImage)

foreign import ccall unsafe "sfImage_CreateFromPixels"
  c_Image_CreateFromPixels :: CUInt -> CUInt -> PixelsPtr ->
                              IO (Ptr AbsImage)

foreign import ccall unsafe "sfImage_CreateFromFile"
  c_Image_CreateFromFile :: CString -> IO (Ptr AbsImage)

foreign import ccall unsafe "sfImage_CreateFromMemory"
  c_Image_CreateFromMemory :: CString -> CSize -> IO (Ptr AbsImage)

foreign import ccall unsafe "sfImage_SaveToFile"
  c_Image_SaveToFile :: Ptr AbsImage -> CString -> IO ()

foreign import ccall unsafe "wrp_sfImage_CreateMaskFromColor"
  c_Image_CreateMaskFromColor :: Ptr AbsImage -> Ptr CColor ->
                                 CColorCoord -> IO ()

foreign import ccall unsafe "wrp_sfImage_Copy"
  c_Image_Copy :: Ptr AbsImage -> Ptr AbsImage ->
                  CUInt -> CUInt -> Ptr (Rect CInt) -> IO ()

foreign import ccall unsafe "wrp_sfImage_CopyScreen"
  c_Image_CopyScreen :: Ptr AbsImage -> Ptr AbsRenderWindow ->
                        Ptr (Rect CInt) -> IO Bool

foreign import ccall unsafe "wrp_sfImage_SetPixel"
  c_Image_SetPixel :: Ptr AbsImage -> CUInt -> CUInt ->
                      Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfImage_GetPixel"
  c_Image_GetPixel :: Ptr AbsImage -> CUInt -> CUInt -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfImage_GetPixelsPtr"
  c_Image_GetPixelsPtr :: Ptr AbsImage -> IO PixelsPtr

foreign import ccall unsafe "sfImage_Bind"
  c_Image_Bind :: Ptr AbsImage -> IO ()

foreign import ccall unsafe "sfImage_SetSmooth"
  c_Image_SetSmooth :: Ptr AbsImage -> Bool -> IO ()

foreign import ccall unsafe "sfImage_GetWidth"
  c_Image_GetWidth :: Ptr AbsImage -> IO CUInt

foreign import ccall unsafe "sfImage_GetHeight"
  c_Image_GetHeight :: Ptr AbsImage -> IO CUInt
 
foreign import ccall unsafe "sfImage_IsSmooth"
  c_Image_IsSmooth :: Ptr AbsImage -> IO Bool

