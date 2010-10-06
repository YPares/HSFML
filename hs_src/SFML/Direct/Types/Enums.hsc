{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

module SFML.Direct.Types.Enums where

import Foreign.C.Types
import qualified Data.Map as M
import qualified Data.Map.Utils as M
import Control.Monad.Identity

import SFML.Direct.Classes.Convertible_HS_C

#include <Wrappers.h>


getDTFromVal mapping val = M.lookup val mapping

getValFromDT mapping dt = head `fmap` M.lookup dt (M.flipM mapping)


type WindowStyleVal = CULong

#{enum WindowStyleVal, ,
  none = sfNone, titlebar = sfTitlebar, resize = sfResize,
  close = sfClose, fullscreen = sfFullscreen
 }

data WindowStyle = None | Titlebar | Resize | Close | Fullscreen
  deriving (Eq, Ord, Enum, Show)

windowStyleMapping = M.fromList
  [(none, None), (titlebar, Titlebar), (resize, Resize), (close, Close),
   (fullscreen, Fullscreen)]

instance (Monad m) => Convertible_HS_C WindowStyle WindowStyleVal m where
  withHs hs f = f $ maybe (error "WindowStyle unknown") id $
                          getValFromDT windowStyleMapping hs
  createFromC c = return $ maybe (error "WindowStyleVal unknown") id $
                             getDTFromVal windowStyleMapping c


type KeyCodeVal = CInt

#{enum KeyCodeVal, ,
  keyA = sfKeyA, keyB = sfKeyB, keyC = sfKeyC, keyD = sfKeyD, keyE = sfKeyE, keyF = sfKeyF,
  keyG = sfKeyG, keyH = sfKeyH, keyI = sfKeyI, keyJ = sfKeyJ, keyK = sfKeyK, keyL = sfKeyL,
  keyM = sfKeyM, keyN = sfKeyN, keyO = sfKeyO, keyP = sfKeyP, keyQ = sfKeyQ, keyR = sfKeyR,
  keyS = sfKeyS, keyT = sfKeyT, keyU = sfKeyU, keyV = sfKeyV, keyW = sfKeyW, keyX = sfKeyX,
  keyY = sfKeyY, keyZ = sfKeyZ,
  keyNum0 = sfKeyNum0, keyNum1 = sfKeyNum1, keyNum2 = sfKeyNum2, keyNum3 = sfKeyNum3,
  keyNum4 = sfKeyNum4, keyNum5 = sfKeyNum5, keyNum6 = sfKeyNum6, keyNum7 = sfKeyNum7,
  keyNum8 = sfKeyNum8, keyNum9 = sfKeyNum9,
  keyEscape = sfKeyEscape, keyLControl = sfKeyLControl, keyLShift = sfKeyLShift,
  keyLAlt = sfKeyLAlt, keyLSystem = sfKeyLSystem, keyRControl = sfKeyRControl,
  keyRShift = sfKeyRShift, keyRAlt = sfKeyRAlt, keyRSystem = sfKeyRSystem,
  keyMenu = sfKeyMenu, keyLBracket = sfKeyLBracket, keyRBracket = sfKeyRBracket,
  keySemiColon = sfKeySemiColon, keyComma = sfKeyComma, keyPeriod = sfKeyPeriod,
  keyQuote = sfKeyQuote, keySlash = sfKeySlash, keyBackSlash = sfKeyBackSlash,
  keyTilde = sfKeyTilde, keyEqual = sfKeyEqual, keyDash = sfKeyDash, keySpace = sfKeySpace,
  keyReturn = sfKeyReturn, keyBack = sfKeyBack, keyTab = sfKeyTab, keyPageUp = sfKeyPageUp,
  keyPageDown = sfKeyPageDown, keyEnd = sfKeyEnd, keyHome = sfKeyHome,
  keyInsert = sfKeyInsert, keyDelete = sfKeyDelete, keyAdd = sfKeyAdd,
  keySubtract = sfKeySubtract, keyMultiply = sfKeyMultiply, keyDivide = sfKeyDivide,
  keyLeft = sfKeyLeft, keyRight = sfKeyRight, keyUp = sfKeyUp, keyDown = sfKeyDown,
  keyNumpad0 = sfKeyNumpad0, keyNumpad1 = sfKeyNumpad1, keyNumpad2 = sfKeyNumpad2,
  keyNumpad3 = sfKeyNumpad3, keyNumpad4 = sfKeyNumpad4, keyNumpad5 = sfKeyNumpad5,
  keyNumpad6 = sfKeyNumpad6, keyNumpad7 = sfKeyNumpad7, keyNumpad8 = sfKeyNumpad8,
  keyNumpad9 = sfKeyNumpad9,
  keyF1 = sfKeyF1, keyF2 = sfKeyF2, keyF3 = sfKeyF3, keyF4 = sfKeyF4,
  keyF5 = sfKeyF5, keyF6 = sfKeyF6, keyF7 = sfKeyF7, keyF8 = sfKeyF8,
  keyF9 = sfKeyF9, keyF10 = sfKeyF10, keyF11 = sfKeyF11, keyF12 = sfKeyF12,
  keyF13 = sfKeyF13, keyF14 = sfKeyF14, keyF15 = sfKeyF15, keyPause = sfKeyPause
 }

data KeyCode = 
  UnknownKeyCode | -- When the key code doesn't match a key constant
  KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ | KeyK | KeyL |
  KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT | KeyU | KeyV | KeyW | KeyX |
  KeyY | KeyZ | KeyNum0 | KeyNum1 | KeyNum2 | KeyNum3 | KeyNum4 | KeyNum5 | KeyNum6 |
  KeyNum7 | KeyNum8 | KeyNum9 | KeyEscape | KeyLControl | KeyLShift | KeyLAlt |
  KeyLSystem | KeyRControl | KeyRShift | KeyRAlt | KeyRSystem | KeyMenu | KeyLBracket |
  KeyRBracket | KeySemiColon | KeyComma | KeyPeriod | KeyQuote | KeySlash | KeyBackSlash |
  KeyTilde | KeyEqual | KeyDash | KeySpace | KeyReturn | KeyBack | KeyTab | KeyPageUp |
  KeyPageDown | KeyEnd | KeyHome | KeyInsert | KeyDelete | KeyAdd | KeySubtract |
  KeyMultiply | KeyDivide | KeyLeft | KeyRight | KeyUp | KeyDown | KeyNumpad0 | KeyNumpad1 |
  KeyNumpad2 | KeyNumpad3 | KeyNumpad4 | KeyNumpad5 | KeyNumpad6 | KeyNumpad7 | KeyNumpad8 |
  KeyNumpad9 | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5 | KeyF6 | KeyF7 | KeyF8 | KeyF9 |
  KeyF10 | KeyF11 | KeyF12 | KeyF13 | KeyF14 | KeyF15 | KeyPause
  deriving (Eq, Ord, Enum, Show)

keyCodeMapping = M.fromList
  [(keyA, KeyA), (keyB, KeyB), (keyC, KeyC), (keyD, KeyD), (keyE, KeyE),
   (keyF, KeyF), (keyG, KeyG), (keyH, KeyH), (keyI, KeyI), (keyJ, KeyJ), 
   (keyK, KeyK), (keyL, KeyL), (keyM, KeyM), (keyN, KeyN), (keyO, KeyO), 
   (keyP, KeyP), (keyQ, KeyQ), (keyR, KeyR), (keyS, KeyS), (keyT, KeyT), 
   (keyU, KeyU), (keyV, KeyV), (keyW, KeyW), (keyX, KeyX), (keyY, KeyY), 
   (keyZ, KeyZ), (keyNum0, KeyNum0), (keyNum1, KeyNum1), (keyNum2, KeyNum2), 
   (keyNum3, KeyNum3), (keyNum4, KeyNum4), (keyNum5, KeyNum5), (keyNum6, KeyNum6), 
   (keyNum7, KeyNum7), (keyNum8, KeyNum8), (keyNum9, KeyNum9), 
   (keyEscape, KeyEscape), (keyLControl, KeyLControl), (keyLShift, KeyLShift), 
   (keyLAlt, KeyLAlt), (keyLSystem, KeyLSystem), (keyRControl, KeyRControl), 
   (keyRShift, KeyRShift), (keyRAlt, KeyRAlt), (keyRSystem, KeyRSystem), 
   (keyMenu, KeyMenu), (keyLBracket, KeyLBracket), (keyRBracket, KeyRBracket), 
   (keySemiColon, KeySemiColon), (keyComma, KeyComma), (keyPeriod, KeyPeriod), 
   (keyQuote, KeyQuote), (keySlash, KeySlash), (keyBackSlash, KeyBackSlash), 
   (keyTilde, KeyTilde), (keyEqual, KeyEqual), (keyDash, KeyDash), 
   (keySpace, KeySpace), (keyReturn, KeyReturn), (keyBack, KeyBack), 
   (keyTab, KeyTab), (keyPageUp, KeyPageUp), (keyPageDown, KeyPageDown), 
   (keyEnd, KeyEnd), (keyHome, KeyHome), (keyInsert, KeyInsert), 
   (keyDelete, KeyDelete), (keyAdd, KeyAdd), (keySubtract, KeySubtract), 
   (keyMultiply, KeyMultiply), (keyDivide, KeyDivide), (keyLeft, KeyLeft), 
   (keyRight, KeyRight), (keyUp, KeyUp), (keyDown, KeyDown), 
   (keyNumpad0, KeyNumpad0), (keyNumpad1, KeyNumpad1), (keyNumpad2, KeyNumpad2), 
   (keyNumpad3, KeyNumpad3), (keyNumpad4, KeyNumpad4), (keyNumpad5, KeyNumpad5), 
   (keyNumpad6, KeyNumpad6), (keyNumpad7, KeyNumpad7), (keyNumpad8, KeyNumpad8), 
   (keyNumpad9, KeyNumpad9), (keyF1, KeyF1), (keyF2, KeyF2), (keyF3, KeyF3), 
   (keyF4, KeyF4), (keyF5, KeyF5), (keyF6, KeyF6), (keyF7, KeyF7), (keyF8, KeyF8), 
   (keyF9, KeyF9), (keyF10, KeyF10), (keyF11, KeyF11), (keyF12, KeyF12), 
   (keyF13, KeyF13), (keyF14, KeyF14), (keyF15, KeyF15), (keyPause, KeyPause)]

instance (Monad m) => Convertible_HS_C KeyCode KeyCodeVal m where
  withHs hs f = f $ maybe (error "KeyCode unknown") id $
                          getValFromDT keyCodeMapping hs
  createFromC c = return $ maybe UnknownKeyCode id $
                             getDTFromVal keyCodeMapping c


type EventTypeVal = CInt

#{enum EventTypeVal, ,
  evtClosed = sfEvtClosed, evtResized = sfEvtResized,
  evtLostFocus = sfEvtLostFocus, evtGainedFocus = sfEvtGainedFocus,
  evtTextEntered = sfEvtTextEntered, evtKeyPressed = sfEvtKeyPressed,
  evtKeyReleased = sfEvtKeyReleased, evtMouseWheelMoved = sfEvtMouseWheelMoved,
  evtMouseButtonPressed = sfEvtMouseButtonPressed,
  evtMouseButtonReleased = sfEvtMouseButtonReleased,
  evtMouseMoved = sfEvtMouseMoved, evtMouseEntered = sfEvtMouseEntered,
  evtMouseLeft = sfEvtMouseLeft, evtJoyButtonPressed = sfEvtJoyButtonPressed,
  evtJoyButtonReleased = sfEvtJoyButtonReleased, evtJoyMoved = sfEvtJoyMoved
 }


type MouseButtonVal = CInt

#{enum MouseButtonVal, ,
  buttonLeft = sfButtonLeft, buttonRight = sfButtonRight, buttonMiddle = sfButtonMiddle,
  buttonX1 = sfButtonX1, buttonX2 = sfButtonX2
 }

data MouseButton = 
  UnknownMouseButton | -- when the button is not one of the following
  ButtonLeft | ButtonRight | ButtonMiddle | ButtonX1 | ButtonX2
  deriving (Eq, Ord, Enum, Show)

mouseButtonMapping = M.fromList
  [(buttonLeft, ButtonLeft), (buttonRight, ButtonRight), (buttonMiddle, ButtonMiddle),
   (buttonX1, ButtonX1), (buttonX2, ButtonX2)]

instance (Monad m) => Convertible_HS_C MouseButton MouseButtonVal m where
  withHs hs f = f $ maybe (error "MouseButton unknown") id $
                          getValFromDT mouseButtonMapping hs
  createFromC c = return $ maybe UnknownMouseButton id $
                             getDTFromVal mouseButtonMapping c


type JoyAxisVal = CInt

#{enum JoyAxisVal, ,
  joyAxisX = sfJoyAxisX, joyAxisY = sfJoyAxisY, joyAxisZ = sfJoyAxisZ,
  joyAxisR = sfJoyAxisR, joyAxisU = sfJoyAxisU, joyAxisV = sfJoyAxisV,
  joyAxisPOV = sfJoyAxisPOV
 }

data JoyAxis = JoyAxisX | JoyAxisY | JoyAxisZ | JoyAxisR | JoyAxisU | JoyAxisV | JoyAxisPOV
  deriving (Eq, Ord, Enum, Show)

joyAxisMapping = M.fromList
  [(joyAxisX, JoyAxisX), (joyAxisY, JoyAxisY), (joyAxisZ, JoyAxisZ),
   (joyAxisR, JoyAxisR), (joyAxisU, JoyAxisU), (joyAxisV, JoyAxisV),
   (joyAxisPOV, JoyAxisPOV)]

instance (Monad m) => Convertible_HS_C JoyAxis JoyAxisVal m where
  withHs hs f = f $ maybe (error "JoyAxis unknown") id $
                          getValFromDT joyAxisMapping hs
  createFromC c = return $ maybe (error "JoyAxisVal unknown") id $
                             getDTFromVal joyAxisMapping c


type BlendModeVal = CInt

#{enum BlendModeVal, ,
  blendAlpha = sfBlendAlpha, blendAdd = sfBlendAdd,
  blendMultiply = sfBlendMultiply, blendNone = sfBlendNone
 }

data BlendMode = BlendAlpha | BlendAdd | BlendMultiply | BlendNone
  deriving (Eq, Ord, Enum, Show)

blendModeMapping = M.fromList
  [(blendAlpha, BlendAlpha), (blendAdd, BlendAdd),
   (blendMultiply, BlendMultiply), (blendNone, BlendNone)]

instance (Monad m) => Convertible_HS_C BlendMode BlendModeVal m where
  withHs hs f = f $ maybe (error "BlendMode unknown") id $
                          getValFromDT blendModeMapping hs
  createFromC c = return $ maybe (error "BlendModeVal unknown") id $
                             getDTFromVal blendModeMapping c


type TextStyleVal = CULong

#{enum TextStyleVal, ,
  textRegular = sfStringRegular, textBold = sfStringBold, textItalic = sfStringItalic,
  textUnderlined = sfStringUnderlined
 }

data TextStyle = TextRegular | TextBold | TextItalic | TextUnderlined
  deriving (Eq, Ord, Enum, Show)

textStyleMapping = M.fromList
  [(textRegular, TextRegular), (textBold, TextBold), (textItalic, TextItalic),
   (textUnderlined, TextUnderlined)]

instance (Monad m) => Convertible_HS_C TextStyle TextStyleVal m where
  withHs hs f = f $ maybe (error "TextStyle unknown") id $
                          getValFromDT textStyleMapping hs
  createFromC c = return $ maybe (error "TextStyleVal unknown") id $
                             getDTFromVal textStyleMapping c

