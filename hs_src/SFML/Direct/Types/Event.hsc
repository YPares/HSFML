{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Event where

import Foreign
import Foreign.C.Types
import qualified Data.Map as M
import Control.Monad.Identity

import SFML.Direct.Types.Enums
import SFML.Direct.Classes.Convertible_HS_C

#include <Wrappers.h>


data AbsEvent

data Event =
  EvtClosed |
  EvtResized {width :: Int, height :: Int} |
  EvtLostFocus |
  EvtGainedFocus |
  EvtTextEntered {unicode :: Int} |
  EvtKeyPressed {code :: KeyCode, alt :: Bool, control :: Bool, shift :: Bool} |
  EvtKeyReleased {code :: KeyCode, alt :: Bool, control :: Bool, shift :: Bool} |
  EvtMouseWheelMoved {delta :: Int} |
  EvtMouseButtonPressed {mouseButton :: MouseButton, mouseX :: Int, mouseY :: Int} |
  EvtMouseButtonReleased {mouseButton :: MouseButton, mouseX :: Int, mouseY :: Int} |
  EvtMouseMoved {mouseX :: Int, mouseY :: Int} |
  EvtMouseEntered {mouseX :: Int, mouseY :: Int} |
  EvtMouseLeft {mouseX :: Int, mouseY :: Int} |
  EvtJoyButtonPressed {joystickId :: Int, joyButton :: Int} |
  EvtJoyButtonReleased {joystickId :: Int, joyButton :: Int} |
  EvtJoyMoved {joystickId :: Int, axis :: JoyAxis, joyPosition :: Float}
  deriving (Show)

makeEventFromPtr :: Ptr AbsEvent -> IO Event
makeEventFromPtr ptr = do
  evtTypeVal <- (#peek sfEvent, Type) ptr
  let action = M.lookup evtTypeVal eventTypeMapping
  maybe (error "Unkown event") id action
  where
    eventTypeMapping = M.fromList
      [(evtClosed, return EvtClosed
       ),
       (evtResized, do
          let sizePtr = (#ptr sfEvent, Size) ptr
          width <- (#peek struct sfSizeEvent, Width) sizePtr
          height <- (#peek struct sfSizeEvent, Height) sizePtr
          return $! EvtResized width height
        ),
       (evtLostFocus, return EvtLostFocus
        ),
       (evtGainedFocus, return EvtGainedFocus
        ),
       (evtTextEntered, do
          unicode <- (#peek struct sfTextEvent, Unicode) .
                     (#ptr sfEvent, Text) $ ptr
          return $! EvtTextEntered unicode
        ),
       (evtKeyPressed, do
          (keyCode,alt,control,shift) <- unmarshallKeyEvent
          return $! EvtKeyPressed keyCode alt control shift
        ),
       (evtKeyReleased, do
          (keyCode,alt,control,shift) <- unmarshallKeyEvent
          return $! EvtKeyReleased keyCode alt control shift
        ),
       (evtMouseWheelMoved, do
          delta <- (#peek struct sfMouseWheelEvent, Delta) . (#ptr sfEvent, MouseWheel) $ ptr
          return $! EvtMouseWheelMoved delta
        ),
       (evtMouseButtonPressed, do
          (button, x, y) <- unmarshallMouseButtonEvent
          return $! EvtMouseButtonPressed button x y
        ),
       (evtMouseButtonReleased, do
          (button, x, y) <- unmarshallMouseButtonEvent
          return $! EvtMouseButtonReleased button x y
        ),
       (evtMouseMoved, do
          (x, y) <- unmarshallMouseMoveEvent
          return $! EvtMouseMoved x y
        ),
       (evtMouseEntered, do
          (x, y) <- unmarshallMouseMoveEvent
          return $! EvtMouseEntered x y
        ),
       (evtMouseLeft, do
          (x, y) <- unmarshallMouseMoveEvent
          return $! EvtMouseLeft x y
        ),
       (evtJoyButtonPressed, do
          (joyId, button) <- unmarshallJoyButtonEvent
          return $! EvtJoyButtonPressed joyId button
        ),
       (evtJoyButtonReleased, do
          (joyId, button) <- unmarshallJoyButtonEvent
          return $! EvtJoyButtonReleased joyId button
        ),
       (evtJoyMoved, do
          let joyPtr = (#ptr sfEvent, JoyMove) ptr
          joyId <- (#peek struct sfJoyMoveEvent, JoystickId) joyPtr
          axisVal <- (#peek struct sfJoyMoveEvent, Axis) joyPtr
          let axis = runIdentity $ createFromC (axisVal :: JoyAxisVal)
          position <- (#peek struct sfJoyMoveEvent, Position) joyPtr
          return $! EvtJoyMoved joyId axis position
        )]

    toBool x = x /= 0

    unmarshallKeyEvent = do
      let keyPtr = (#ptr sfEvent, Key) ptr
      keyCodeVal <- (#peek struct sfKeyEvent, Code) keyPtr
      let keyCode = runIdentity $ createFromC (keyCodeVal :: KeyCodeVal)
      alt <- (#peek struct sfKeyEvent, Alt) keyPtr
      control <- (#peek struct sfKeyEvent, Control) keyPtr
      shift <- (#peek struct sfKeyEvent, Shift) keyPtr
      return (keyCode, toBool (alt :: CInt), toBool (control :: CInt), toBool (shift :: CInt))

    unmarshallMouseButtonEvent = do
      let buttonPtr = (#ptr sfEvent, MouseButton) ptr
      buttonVal <- (#peek struct sfMouseButtonEvent, Button) buttonPtr
      let button = runIdentity $ createFromC (buttonVal :: MouseButtonVal)
      x <- (#peek struct sfMouseButtonEvent, X) buttonPtr
      y <- (#peek struct sfMouseButtonEvent, Y) buttonPtr
      return (button, x, y)

    unmarshallMouseMoveEvent = do
      let movePtr = (#ptr sfEvent, MouseMove) ptr
      x <- (#peek struct sfMouseMoveEvent, X) movePtr
      y <- (#peek struct sfMouseMoveEvent, Y) movePtr
      return (x, y)

    unmarshallJoyButtonEvent = do
      let joyPtr = (#ptr sfEvent, JoyButton) ptr
      joyId <- (#peek struct sfJoyButtonEvent, JoystickId) joyPtr
      button <- (#peek struct sfJoyButtonEvent, Button) joyPtr
      return (joyId, button)

