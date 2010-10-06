{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.View where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.View
import SFML.Direct.Types.Rect


foreign import ccall unsafe "sfView_Create"
  c_View_Create :: IO (Ptr AbsView)

foreign import ccall unsafe "wrp_sfView_CreateFromRect"
  c_View_CreateFromRect :: Ptr (Rect CFloat) -> IO (Ptr AbsView)

foreign import ccall unsafe "sfView_SetCenter"
  c_View_SetCenter :: Ptr AbsView -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfView_SetHalfSize"
  c_View_SetHalfSize :: Ptr AbsView -> CFloat -> CFloat -> IO ()

--sfView_SetFromRect will not be bound

foreign import ccall unsafe "sfView_GetCenterX"
  c_View_GetCenterX :: Ptr AbsView -> IO CFloat

foreign import ccall unsafe "sfView_GetCenterY"
  c_View_GetCenterY :: Ptr AbsView -> IO CFloat

foreign import ccall unsafe "sfView_GetHalfSizeX"
  c_View_GetHalfSizeX :: Ptr AbsView -> IO CFloat

foreign import ccall unsafe "sfView_GetHalfSizeY"
  c_View_GetHalfSizeY :: Ptr AbsView -> IO CFloat

foreign import ccall unsafe "wrp_sfView_GetRect"
  c_View_GetRect :: Ptr AbsView -> Ptr (Rect CFloat) -> IO ()

foreign import ccall unsafe "sfView_Move"
  c_View_Move :: Ptr AbsView -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfView_Zoom"
  c_View_Zoom :: Ptr AbsView -> CFloat -> IO ()

