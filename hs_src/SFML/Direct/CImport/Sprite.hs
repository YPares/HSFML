{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Sprite where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Sprite
import SFML.Direct.Types.Image
import SFML.Direct.Types.Color
import SFML.Direct.Types.Enums
import SFML.Direct.Types.Rect


foreign import ccall unsafe "sfSprite_Create"
  c_Sprite_Create :: IO (Ptr AbsSprite)
  
foreign import ccall unsafe "sfSprite_SetX"
  c_Sprite_SetX :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetY"
  c_Sprite_SetY :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetPosition"
  c_Sprite_SetPosition :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetScaleX"
  c_Sprite_SetScaleX :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetScaleY"
  c_Sprite_SetScaleY :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetScale"
  c_Sprite_SetScale :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetRotation"
  c_Sprite_SetRotation :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetCenter"
  c_Sprite_SetCenter :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "wrp_sfSprite_SetColor"
  c_Sprite_SetColor :: Ptr AbsSprite -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfSprite_SetBlendMode"
  c_Sprite_SetBlendMode :: Ptr AbsSprite -> BlendModeVal -> IO ()

foreign import ccall unsafe "sfSprite_GetX"
  c_Sprite_GetX :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetY"
  c_Sprite_GetY :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetScaleX"
  c_Sprite_GetScaleX :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetScaleY"
  c_Sprite_GetScaleY :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetRotation"
  c_Sprite_GetRotation :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetCenterX"
  c_Sprite_GetCenterX :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetCenterY"
  c_Sprite_GetCenterY :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "wrp_sfSprite_GetColor"
  c_Sprite_GetColor :: Ptr AbsSprite -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfSprite_GetBlendMode"
  c_Sprite_GetBlendMode :: Ptr AbsSprite -> IO BlendModeVal

foreign import ccall unsafe "sfSprite_Move"
  c_Sprite_Move :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_Scale"
  c_Sprite_Scale :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_Rotate"
  c_Sprite_Rotate :: Ptr AbsSprite -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_TransformToLocal"
  c_Sprite_TransformToLocal :: Ptr AbsSprite -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "sfSprite_TransformToGlobal"
  c_Sprite_TransformToGlobal :: Ptr AbsSprite -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "sfSprite_SetImage"
  c_Sprite_SetImage :: Ptr AbsSprite -> Ptr AbsImage -> IO ()

foreign import ccall unsafe "wrp_sfSprite_SetSubRect"
  c_Sprite_SetSubRect :: Ptr AbsSprite -> Ptr (Rect CInt) -> IO ()

foreign import ccall unsafe "sfSprite_Resize"
  c_Sprite_Resize :: Ptr AbsSprite -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfSprite_FlipX"
  c_Sprite_FlipX :: Ptr AbsSprite -> Bool -> IO ()

foreign import ccall unsafe "sfSprite_FlipY"
  c_Sprite_FlipY :: Ptr AbsSprite -> Bool -> IO ()

foreign import ccall unsafe "sfSprite_GetImage"
  c_Sprite_GetImage :: Ptr AbsSprite -> IO (Ptr AbsImage)

foreign import ccall unsafe "wrp_sfSprite_GetSubRect"
  c_Sprite_GetSubRect :: Ptr AbsSprite -> Ptr (Rect CInt) -> IO ()

foreign import ccall unsafe "sfSprite_GetWidth"
  c_Sprite_GetWidth :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetHeight"
  c_Sprite_GetHeight :: Ptr AbsSprite -> IO CFloat

foreign import ccall unsafe "sfSprite_GetPixel"
  c_Sprite_GetPixel :: Ptr AbsSprite -> CUInt -> CUInt ->
                       Ptr CColor -> IO ()

