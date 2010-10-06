{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Input where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Input
import SFML.Direct.Types.Enums


foreign import ccall unsafe "sfInput_IsKeyDown"
  c_Input_IsKeyDown :: Input -> KeyCodeVal -> IO Bool

foreign import ccall unsafe "sfInput_IsMouseButtonDown"
  c_Input_IsMouseButtonDown :: Input -> MouseButtonVal -> IO Bool

foreign import ccall unsafe "sfInput_IsJoystickButtonDown"
  c_Input_IsJoystickButtonDown :: Input -> CUInt -> IO Bool

foreign import ccall unsafe "sfInput_GetMouseX"
  c_Input_GetMouseX :: Input -> IO CInt
  
foreign import ccall unsafe "sfInput_GetMouseY"
  c_Input_GetMouseY :: Input -> IO CInt

foreign import ccall unsafe "sfInput_GetJoystickAxis"
  c_Input_GetJoystickAxis :: Input -> CUInt -> JoyAxisVal -> IO CFloat

