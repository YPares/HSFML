{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Graphics.View where

import Foreign
import Foreign.C.Types
import Control.Monad (liftM2)

import SFML.Direct.CImport.View
import SFML.Direct.Types.View
import SFML.Direct.Types.Rect
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling


instance HasCenter View Float IO where
  getCenter view = withHs view $ \viewPtr ->
      let get f = realToFrac `fmap` f viewPtr
      in liftM2 (,) (get c_View_GetCenterX) (get c_View_GetCenterY)

  setCenter view (x, y) = withHs view $ \viewPtr -> do
      c_View_SetCenter viewPtr (realToFrac x) (realToFrac y)

instance Movable View Float IO where
  move view (offsetX, offsetY) =
    withHs view $ \viewPtr -> do
      c_View_Move viewPtr (realToFrac offsetX) (realToFrac offsetY)

createView :: IO View
createView = c_View_Create >>= createFromC

createViewFromRect :: Rect Float -> IO View
createViewFromRect rect = withHs rect $ \rectPtr ->
    c_View_CreateFromRect rectPtr >>= createFromC

setHalfSize :: View -> Float -> Float -> IO ()
setHalfSize view halfW halfH =
  withHs view $ \viewPtr -> do
    c_View_SetHalfSize viewPtr (realToFrac halfW) (realToFrac halfH)

--setFromRect will not be bound : since we use here views in a pure functional context, setFromRect is equivalent to createFromRect

getHalfSize :: View -> IO (Float, Float)
getHalfSize view =
  withHs view $ \viewPtr ->
    let get f = realToFrac `fmap` f viewPtr
    in liftM2 (,) (get c_View_GetHalfSizeX) (get c_View_GetHalfSizeY)

instance BoundedByRect View Float IO where
  getRect view =
    withHs view $ \viewPtr ->
      withHs (Rect 0 0 0 0 :: Rect Float) $ \rectPtr -> do
        c_View_GetRect viewPtr rectPtr
        createFromC rectPtr

zoom :: View -> Float -> IO ()
zoom view factor =
  withHs view $ \viewPtr -> do
    c_View_Zoom viewPtr (realToFrac factor)

