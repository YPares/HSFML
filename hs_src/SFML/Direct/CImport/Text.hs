{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Text where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import SFML.Direct.Types.Text
import SFML.Direct.Types.Color
import SFML.Direct.Types.Rect
import SFML.Direct.Types.Enums
import SFML.Direct.Types.Font
import SFML.Direct.Types.SfInts


foreign import ccall unsafe "sfString_Create"
  c_Text_Create :: IO (Ptr AbsText)
  
foreign import ccall unsafe "sfString_SetX"
  c_Text_SetX :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetY"
  c_Text_SetY :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetPosition"
  c_Text_SetPosition :: Ptr AbsText -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetScaleX"
  c_Text_SetScaleX :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetScaleY"
  c_Text_SetScaleY :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetScale"
  c_Text_SetScale :: Ptr AbsText -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetRotation"
  c_Text_SetRotation :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetCenter"
  c_Text_SetCenter :: Ptr AbsText -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "wrp_sfString_SetColor"
  c_Text_SetColor :: Ptr AbsText -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfString_SetBlendMode"
  c_Text_SetBlendMode :: Ptr AbsText -> BlendModeVal -> IO ()

foreign import ccall unsafe "sfString_GetX"
  c_Text_GetX :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetY"
  c_Text_GetY :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetScaleX"
  c_Text_GetScaleX :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetScaleY"
  c_Text_GetScaleY :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetRotation"
  c_Text_GetRotation :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetCenterX"
  c_Text_GetCenterX :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetCenterY"
  c_Text_GetCenterY :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "wrp_sfString_GetColor"
  c_Text_GetColor :: Ptr AbsText -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfString_GetBlendMode"
  c_Text_GetBlendMode :: Ptr AbsText -> IO BlendModeVal

foreign import ccall unsafe "sfString_Move"
  c_Text_Move :: Ptr AbsText -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfString_Scale"
  c_Text_Scale :: Ptr AbsText -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfString_Rotate"
  c_Text_Rotate :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_TransformToLocal"
  c_Text_TransformToLocal :: Ptr AbsText -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "sfString_TransformToGlobal"
  c_Text_TransformToGlobal :: Ptr AbsText -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()


foreign import ccall unsafe "sfString_SetText"
  c_Text_SetString :: Ptr AbsText -> CString -> IO ()

foreign import ccall unsafe "sfString_SetUnicodeText"
  c_Text_SetUnicodeString :: Ptr AbsText -> Ptr SfUint32 -> IO ()

foreign import ccall unsafe "sfString_SetFont"
  c_Text_SetFont :: Ptr AbsText -> Ptr AbsFont -> IO ()

foreign import ccall unsafe "sfString_SetSize"
  c_Text_SetSize :: Ptr AbsText -> CFloat -> IO ()

foreign import ccall unsafe "sfString_SetStyle"
  c_Text_SetStyle :: Ptr AbsText -> CULong -> IO ()

foreign import ccall unsafe "sfString_GetUnicodeText"
  c_Text_GetUnicodeString :: Ptr AbsText -> IO (Ptr SfUint32)

foreign import ccall unsafe "sfString_GetText"
  c_Text_GetString :: Ptr AbsText -> IO CString

foreign import ccall unsafe "sfString_GetFont"
  c_Text_GetFont :: Ptr AbsText -> IO (Ptr AbsFont)

foreign import ccall unsafe "sfString_GetSize"
  c_Text_GetSize :: Ptr AbsText -> IO CFloat

foreign import ccall unsafe "sfString_GetStyle"
  c_Text_GetStyle :: Ptr AbsText -> IO CULong

foreign import ccall unsafe "sfString_GetCharacterPos"
  c_Text_GetCharacterPos :: Ptr AbsText -> CSize -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "wrp_sfString_GetRect"
  c_Text_GetRect :: Ptr AbsText -> Ptr (Rect CFloat) -> IO ()

