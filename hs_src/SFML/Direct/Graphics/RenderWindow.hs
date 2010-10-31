{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module SFML.Direct.Graphics.RenderWindow where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Foldable as F

import SFML.Direct.CImport.RenderWindow
import SFML.Direct.Types.RenderWindow
import SFML.Direct.Types.VideoMode
import SFML.Direct.Types.Event
import SFML.Direct.Types.Input
import SFML.Direct.Types.Enums
import SFML.Direct.Types.PostFX
import SFML.Direct.Types.Color
import SFML.Direct.Types.Image
import SFML.Direct.Types.View
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.WindowClass
import SFML.Direct.Classes.Handling
import SFML.Direct.Classes.Drawable


instance Sized RenderWindow Int IO where
  getSize w =
    let getDim f = fromIntegral `fmap` withHs w f
    in liftM2 (,) (getDim c_RenderWindow_GetWidth)
                  (getDim c_RenderWindow_GetHeight)
  
instance Resizable RenderWindow Int IO where
  setSize w (left, top) = withHs w $ \ptr ->
    c_RenderWindow_SetSize ptr (fromIntegral left) (fromIntegral top)

instance Positionable RenderWindow Int IO where
  setPosition w (left, top) = withHs w $ \ptr ->
    c_RenderWindow_SetPosition ptr (fromIntegral left) (fromIntegral top)

instance WindowClass RenderWindow IO where
  createWindow mode title styles settings =
    withHs mode $ \modePtr ->
      withHs settings $ \settingsPtr ->
        withCString title $ \titleCStr ->
          c_RenderWindow_Create modePtr titleCStr convStyles settingsPtr >>=
          maybeCreateFromC
    
    where convStyles | null styles = none
                     | otherwise   = foldr (.|.) 0 styleVals
            where styleVals = flip map styles $ \s ->
                                runIdentity $ withHs s return

  close w = withHs w c_RenderWindow_Close

  isOpened w = withHs w c_RenderWindow_IsOpened

  getSettings w =
    withHs w $ \wPtr ->
      withHs (0::Int, 0::Int, 0::Int) $ \settingsPtr -> do
        c_RenderWindow_GetSettings wPtr settingsPtr
        createFromC settingsPtr

  getEvent (RenderWindow winFp evtFp) =
    withForeignPtr winFp $ \winPtr ->
      withForeignPtr evtFp $ \evtPtr -> do
        evtOccured <- c_RenderWindow_GetEvent winPtr evtPtr
        if evtOccured
          then Just `fmap` makeEventFromPtr evtPtr
          else return Nothing

  useVerticalSync w b = withHs w (flip c_RenderWindow_UseVerticalSync b)

  showMouseCursor w b = withHs w (flip c_RenderWindow_ShowMouseCursor b)

  setCursorPosition w (left, top) = withHs w $ \ptr ->
    c_RenderWindow_SetCursorPosition ptr (fromIntegral left) (fromIntegral top)

  showWindow w b = withHs w (flip c_RenderWindow_Show b)

  enableKeyRepeat w b = withHs w (flip c_RenderWindow_EnableKeyRepeat b)

  setActive w b = withHs w (flip c_RenderWindow_SetActive b)

  display w = withHs w c_RenderWindow_Display

  getInput w = withHs w c_RenderWindow_GetInput >>= createFromC

  setFramerateLimit w limit =
    withHs w (flip c_RenderWindow_SetFramerateLimit $ fromIntegral limit)

  getFrameTime w = realToFrac `fmap` withHs w c_RenderWindow_GetFrameTime

  setJoystickThreshold w thr =
    withHs w (flip c_RenderWindow_SetJoystickThreshold $ realToFrac thr)


draw :: (Drawable obj dim IO) => RenderWindow -> obj -> IO ()
draw w obj =
  withHs w $ \wPtr ->
    drawFunc obj wPtr

drawSeq :: RenderWindow -> DrawableSeq dim IO -> IO ()
drawSeq w seq =
  F.mapM_ (\(DrawableEDT obj) -> draw w obj) seq

drawPostFX :: RenderWindow -> PostFX -> IO ()
drawPostFX w obj =
  withHs w $ \wPtr ->
    withHs obj $ \objPtr ->
      c_RenderWindow_DrawPostFX wPtr objPtr

capture :: RenderWindow -> IO Image
capture w =
  withHs w $ \ptr ->
    c_RenderWindow_Capture ptr >>= createFromC

clear :: RenderWindow -> Color -> IO ()
clear w clr =
  withHs w $ \ptr ->
    withHs clr $ \clrPtr ->
      c_RenderWindow_Clear ptr clrPtr

setView :: RenderWindow -> View -> IO ()
setView w view =
  withHs w $ \wPtr ->
    withHs view $ \viewPtr ->
      c_RenderWindow_SetView wPtr viewPtr

getView :: RenderWindow -> IO View
getView w =
  withHs w $ \wPtr ->
    c_RenderWindow_GetView wPtr >>= createFromC_

getDefaultView :: RenderWindow -> IO View
getDefaultView w =
  withHs w $ \wPtr ->
    c_RenderWindow_GetDefaultView wPtr >>= createFromC_

convertCoords :: RenderWindow -> (Int, Int) -> Maybe View -> IO (Float, Float)
convertCoords w (winX, winY) maybeTargetView =
  withHs w $ \wPtr ->
    with 0 $ \viewXPtr ->
      with 0 $ \viewYPtr ->
        maybeWith withHs maybeTargetView $ \viewPtr -> do
          c_RenderWindow_ConvertCoords wPtr (fromIntegral winX) (fromIntegral winY) viewXPtr viewYPtr viewPtr
          liftM2 (,) (realToFrac `fmap` peek viewXPtr)
                     (realToFrac `fmap` peek viewYPtr)

preserveOpenGLStates :: RenderWindow -> Bool -> IO ()
preserveOpenGLStates w b =
  withHs w (flip c_RenderWindow_PreserveOpenGLStates b)

