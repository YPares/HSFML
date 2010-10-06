module SFML.Direct.Window.Input where

import Foreign
import Control.Monad (liftM2)

import SFML.Direct.CImport.Input
import SFML.Direct.Types.Input
import SFML.Direct.Types.Enums
import SFML.Direct.Classes.Convertible_HS_C


isKeyDown :: Input -> KeyCode -> IO Bool
isKeyDown input keyCode =
  withHs input $ \cInput ->
    withHs keyCode $ \cKeyCode ->
      c_Input_IsKeyDown cInput cKeyCode

isMouseButtonDown :: Input -> MouseButton -> IO Bool
isMouseButtonDown input mouseButton =
  withHs input $ \cInput ->
    withHs mouseButton $ \cButton ->
      c_Input_IsMouseButtonDown cInput cButton

isJoystickButtonDown :: Input -> Int -> IO Bool
isJoystickButtonDown input jstickButton =
  withHs input $ \cInput ->
    c_Input_IsJoystickButtonDown cInput (fromIntegral jstickButton)

getMousePosition :: Input -> IO (Int, Int)
getMousePosition input =
  withHs input $ \cInput ->
    let getCoord f = fromIntegral `fmap` f cInput
    in liftM2 (,) (getCoord c_Input_GetMouseX) (getCoord c_Input_GetMouseY)

getJoystickAxis :: Input -> Int -> JoyAxis -> IO Float
getJoystickAxis input jstickId joyAxis =
  withHs input $ \cInput ->
    withHs joyAxis $ \cJoyAxis ->
      realToFrac `fmap`
        c_Input_GetJoystickAxis cInput (fromIntegral jstickId) cJoyAxis

