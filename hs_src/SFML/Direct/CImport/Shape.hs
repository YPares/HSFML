{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Shape where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Shape
import SFML.Direct.Types.Color
import SFML.Direct.Types.Enums


foreign import ccall unsafe "sfShape_Create"
  c_Shape_Create :: IO (Ptr AbsShape)
  
foreign import ccall unsafe "wrp_sfShape_CreateLine"
  c_Shape_CreateLine :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> Ptr CColor -> CFloat -> Ptr CColor -> IO (Ptr AbsShape)

foreign import ccall unsafe "wrp_sfShape_CreateRectangle"
  c_Shape_CreateRectangle :: CFloat -> CFloat -> CFloat -> CFloat -> Ptr CColor -> CFloat -> Ptr CColor -> IO (Ptr AbsShape)

foreign import ccall unsafe "wrp_sfShape_CreateCircle"
  c_Shape_CreateCircle :: CFloat -> CFloat -> CFloat -> Ptr CColor -> CFloat -> Ptr CColor -> IO (Ptr AbsShape)


foreign import ccall unsafe "sfShape_SetX"
  c_Shape_SetX :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetY"
  c_Shape_SetY :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetPosition"
  c_Shape_SetPosition :: Ptr AbsShape -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetScaleX"
  c_Shape_SetScaleX :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetScaleY"
  c_Shape_SetScaleY :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetScale"
  c_Shape_SetScale :: Ptr AbsShape -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetRotation"
  c_Shape_SetRotation :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_SetCenter"
  c_Shape_SetCenter :: Ptr AbsShape -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "wrp_sfShape_SetColor"
  c_Shape_SetColor :: Ptr AbsShape -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfShape_SetBlendMode"
  c_Shape_SetBlendMode :: Ptr AbsShape -> BlendModeVal -> IO ()

foreign import ccall unsafe "sfShape_GetX"
  c_Shape_GetX :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetY"
  c_Shape_GetY :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetScaleX"
  c_Shape_GetScaleX :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetScaleY"
  c_Shape_GetScaleY :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetRotation"
  c_Shape_GetRotation :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetCenterX"
  c_Shape_GetCenterX :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetCenterY"
  c_Shape_GetCenterY :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "wrp_sfShape_GetColor"
  c_Shape_GetColor :: Ptr AbsShape -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfShape_GetBlendMode"
  c_Shape_GetBlendMode :: Ptr AbsShape -> IO BlendModeVal

foreign import ccall unsafe "sfShape_Move"
  c_Shape_Move :: Ptr AbsShape -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_Scale"
  c_Shape_Scale :: Ptr AbsShape -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_Rotate"
  c_Shape_Rotate :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_TransformToLocal"
  c_Shape_TransformToLocal :: Ptr AbsShape -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "sfShape_TransformToGlobal"
  c_Shape_TransformToGlobal :: Ptr AbsShape -> CFloat -> CFloat -> 
                               Ptr CFloat -> Ptr CFloat -> IO ()


foreign import ccall unsafe "wrp_sfShape_AddPoint"
  c_Shape_AddPoint :: Ptr AbsShape -> CFloat -> CFloat -> Ptr CColor -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfShape_EnableFill"
  c_Shape_EnableFill :: Ptr AbsShape -> Bool -> IO ()

foreign import ccall unsafe "sfShape_EnableOutline"
  c_Shape_EnableOutline :: Ptr AbsShape -> Bool -> IO ()

foreign import ccall unsafe "sfShape_SetOutlineWidth"
  c_Shape_SetOutlineWidth :: Ptr AbsShape -> CFloat -> IO ()

foreign import ccall unsafe "sfShape_GetOutlineWidth"
  c_Shape_GetOutlineWidth :: Ptr AbsShape -> IO CFloat

foreign import ccall unsafe "sfShape_GetNbPoints"
  c_Shape_GetNbPoints :: Ptr AbsShape -> IO CUInt

foreign import ccall unsafe "sfShape_GetPointPosition"
  c_Shape_GetPointPosition :: Ptr AbsShape -> CUInt -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "wrp_sfShape_GetPointColor"
  c_Shape_GetPointColor :: Ptr AbsShape -> CUInt -> Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfShape_GetPointOutlineColor"
  c_Shape_GetPointOutlineColor :: Ptr AbsShape -> CUInt -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfShape_SetPointPosition"
  c_Shape_SetPointPosition :: Ptr AbsShape -> CUInt -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "wrp_sfShape_SetPointColor"
  c_Shape_SetPointColor :: Ptr AbsShape -> CUInt -> Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfShape_SetPointOutlineColor"
  c_Shape_SetPointOutlineColor :: Ptr AbsShape -> CUInt -> Ptr CColor -> IO ()

