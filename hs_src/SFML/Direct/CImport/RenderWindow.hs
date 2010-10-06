{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module SFML.Direct.CImport.RenderWindow where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import SFML.Direct.Types.RenderWindow
import SFML.Direct.Types.VideoMode
import SFML.Direct.Types.Event
import SFML.Direct.Types.Enums
import SFML.Direct.Types.Image
import SFML.Direct.Types.Input
import SFML.Direct.Types.Sprite
import SFML.Direct.Types.PostFX
import SFML.Direct.Types.Shape
import SFML.Direct.Types.Text
import SFML.Direct.Types.Color
import SFML.Direct.Types.View


foreign import ccall unsafe "wrp_sfRenderWindow_Create"
  c_RenderWindow_Create :: Ptr (CUInt, CUInt, CUInt) -> CString -> WindowStyleVal -> Ptr (CUInt, CUInt, CUInt) -> IO (Ptr AbsRenderWindow)

--sfRenderWindow_CreateFromHandle

foreign import ccall unsafe "sfRenderWindow_Close"
  c_RenderWindow_Close :: Ptr AbsRenderWindow -> IO ()

foreign import ccall unsafe "sfRenderWindow_IsOpened"
  c_RenderWindow_IsOpened :: Ptr AbsRenderWindow -> IO Bool

foreign import ccall unsafe "sfRenderWindow_GetWidth"
  c_RenderWindow_GetWidth :: Ptr AbsRenderWindow -> IO CUInt

foreign import ccall unsafe "sfRenderWindow_GetHeight"
  c_RenderWindow_GetHeight :: Ptr AbsRenderWindow -> IO CUInt

foreign import ccall unsafe "wrp_sfRenderWindow_GetSettings"
  c_RenderWindow_GetSettings :: Ptr AbsRenderWindow -> Ptr (CUInt, CUInt, CUInt) -> IO ()

foreign import ccall unsafe "sfRenderWindow_GetEvent"
  c_RenderWindow_GetEvent :: Ptr AbsRenderWindow -> Ptr AbsEvent -> IO Bool

foreign import ccall unsafe "sfRenderWindow_UseVerticalSync"
  c_RenderWindow_UseVerticalSync :: Ptr AbsRenderWindow -> Bool -> IO ()

foreign import ccall unsafe "sfRenderWindow_ShowMouseCursor"
  c_RenderWindow_ShowMouseCursor :: Ptr AbsRenderWindow -> Bool -> IO ()

foreign import ccall unsafe "sfRenderWindow_SetCursorPosition"
  c_RenderWindow_SetCursorPosition :: Ptr AbsRenderWindow -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "sfRenderWindow_SetPosition"
  c_RenderWindow_SetPosition :: Ptr AbsRenderWindow -> CInt -> CInt -> IO ()

foreign import ccall unsafe "sfRenderWindow_SetSize"
  c_RenderWindow_SetSize :: Ptr AbsRenderWindow -> CUInt -> CUInt -> IO ()

foreign import ccall unsafe "sfRenderWindow_Show"
  c_RenderWindow_Show :: Ptr AbsRenderWindow -> Bool -> IO ()

foreign import ccall unsafe "sfRenderWindow_EnableKeyRepeat"
  c_RenderWindow_EnableKeyRepeat :: Ptr AbsRenderWindow -> Bool -> IO ()

--sfRenderWindow_SetIcon

foreign import ccall unsafe "sfRenderWindow_SetActive"
  c_RenderWindow_SetActive :: Ptr AbsRenderWindow -> Bool -> IO ()

-- | Calls to 'sleep'
foreign import ccall safe "sfRenderWindow_Display"
  c_RenderWindow_Display :: Ptr AbsRenderWindow -> IO ()

foreign import ccall unsafe "sfRenderWindow_GetInput"
  c_RenderWindow_GetInput :: Ptr AbsRenderWindow -> IO Input

foreign import ccall unsafe "sfRenderWindow_SetFramerateLimit"
  c_RenderWindow_SetFramerateLimit :: Ptr AbsRenderWindow -> CUInt -> IO ()

foreign import ccall unsafe "sfRenderWindow_GetFrameTime"
  c_RenderWindow_GetFrameTime :: Ptr AbsRenderWindow -> IO CFloat

foreign import ccall unsafe "sfRenderWindow_SetJoystickThreshold"
  c_RenderWindow_SetJoystickThreshold :: Ptr AbsRenderWindow -> CFloat -> IO ()

foreign import ccall unsafe "sfRenderWindow_DrawPostFX"
  c_RenderWindow_DrawPostFX :: Ptr AbsRenderWindow -> Ptr AbsPostFX -> IO ()

foreign import ccall unsafe "sfRenderWindow_DrawSprite"
  c_RenderWindow_DrawSprite :: Ptr AbsRenderWindow -> Ptr AbsSprite -> IO ()

foreign import ccall unsafe "sfRenderWindow_DrawShape"
  c_RenderWindow_DrawShape :: Ptr AbsRenderWindow -> Ptr AbsShape -> IO ()

foreign import ccall unsafe "sfRenderWindow_DrawString"
  c_RenderWindow_DrawText :: Ptr AbsRenderWindow -> Ptr AbsText -> IO ()

foreign import ccall unsafe "sfRenderWindow_Capture"
  c_RenderWindow_Capture :: Ptr AbsRenderWindow -> IO (Ptr AbsImage)

foreign import ccall unsafe "wrp_sfRenderWindow_Clear"
  c_RenderWindow_Clear :: Ptr AbsRenderWindow -> Ptr CColor -> IO ()

foreign import ccall unsafe "sfRenderWindow_SetView"
  c_RenderWindow_SetView :: Ptr AbsRenderWindow -> Ptr AbsView -> IO ()

foreign import ccall unsafe "sfRenderWindow_GetView"
  c_RenderWindow_GetView :: Ptr AbsRenderWindow -> IO (Ptr AbsView) --const

foreign import ccall unsafe "sfRenderWindow_GetDefaultView"
  c_RenderWindow_GetDefaultView :: Ptr AbsRenderWindow -> IO (Ptr AbsView)

foreign import ccall unsafe "sfRenderWindow_ConvertCoords"
  c_RenderWindow_ConvertCoords :: Ptr AbsRenderWindow -> CUInt -> CUInt -> Ptr CFloat -> Ptr CFloat -> Ptr AbsView -> IO ()

foreign import ccall unsafe "sfRenderWindow_PreserveOpenGLStates"
  c_RenderWindow_PreserveOpenGLStates :: Ptr AbsRenderWindow -> Bool -> IO ()

