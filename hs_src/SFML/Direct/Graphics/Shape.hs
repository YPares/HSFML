{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Direct.Graphics.Shape where

import Foreign
import Foreign.C.Types
import Control.Monad (liftM2)

import SFML.Direct.CImport.Shape
import SFML.Direct.CImport.RenderWindow
import SFML.Direct.Types.Shape
import SFML.Direct.Types.Color
import SFML.Direct.Types.Enums
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling
import SFML.Direct.Classes.Drawable


instance Positionable Shape Float IO where
  setPosition shape (x, y) =
    withHs shape $ \ptr ->
      c_Shape_SetPosition ptr (realToFrac x) (realToFrac y)

instance HasCenter Shape Float IO where
  setCenter shape (x, y) =
    withHs shape $ \ptr ->
      c_Shape_SetCenter ptr (realToFrac x) (realToFrac y)

  getCenter shape =
    withHs shape $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Shape_GetCenterX) (get c_Shape_GetCenterY)

instance Movable Shape Float IO where
  move shape (x, y) =
    withHs shape $ \ptr ->
      c_Shape_Move ptr (realToFrac x) (realToFrac y)

instance Drawable Shape Float IO where
  setX shape x =
    withHs shape $ \ptr ->
      c_Shape_SetX ptr (realToFrac x)

  setY shape y =
    withHs shape $ \ptr ->
      c_Shape_SetY ptr (realToFrac y)

  setScaleX shape x =
    withHs shape $ \ptr ->
      c_Shape_SetScaleX ptr (realToFrac x)

  setScaleY shape y =
    withHs shape $ \ptr ->
      c_Shape_SetScaleY ptr (realToFrac y)

  setScale shape (x, y) =
    withHs shape $ \ptr ->
      c_Shape_SetScale ptr (realToFrac x) (realToFrac y)

  setRotation shape rot =
    withHs shape $ \ptr ->
      c_Shape_SetRotation ptr (realToFrac rot)

  setColor shape color =
    withHs shape $ \ptr ->
      withHs color $ \clrPtr ->
        c_Shape_SetColor ptr clrPtr
   
  setBlendMode shape bm =
    withHs shape $ \ptr ->
      withHs bm $ \cBM ->
        c_Shape_SetBlendMode ptr cBM

  getPosition shape =
    withHs shape $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Shape_GetX) (get c_Shape_GetY)

  getScale shape =
    withHs shape $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Shape_GetScaleX) (get c_Shape_GetScaleY)

  getRotation shape =
    withHs shape $ \ptr ->
      realToFrac `fmap` c_Shape_GetRotation ptr

  getColor shape =
    withHs shape $ \ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
        c_Shape_GetColor ptr clrPtr
        createFromC clrPtr

  getBlendMode shape =
    withHs shape $ \ptr ->
      c_Shape_GetBlendMode ptr >>= createFromC
      
  scale shape (x, y) =
    withHs shape $ \ptr ->
      c_Shape_Scale ptr (realToFrac x) (realToFrac y)

  rotate shape angle =
    withHs shape $ \ptr ->
      c_Shape_Rotate ptr (realToFrac angle)

  transformToLocal shape (ptX, ptY) =
    withHs shape $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Shape_TransformToLocal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)

  transformToGlobal shape (ptX, ptY) =
    withHs shape $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Shape_TransformToGlobal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)

  drawFunc shape wPtr = 
    withHs shape $ \shapePtr ->
      c_RenderWindow_DrawShape wPtr shapePtr


createShape :: IO Shape
createShape = c_Shape_Create >>= createFromC

createLine :: (Float, Float) -> (Float, Float) -> Float -> Color -> Float -> Color -> IO Shape
createLine (p1X, p1Y) (p2X, p2Y) thickness col outline outlineCol =
  withHs col $ \cCol ->
    withHs outlineCol $ \cOutlineCol ->
      c_Shape_CreateLine (realToFrac p1X) (realToFrac p1Y) (realToFrac p2X) (realToFrac p2Y) (realToFrac thickness) cCol (realToFrac outline) cOutlineCol >>= createFromC

createRectangle :: (Float, Float) -> (Float, Float) -> Color -> Float -> Color -> IO Shape
createRectangle (p1X, p1Y) (p2X, p2Y) col outline outlineCol =
  withHs col $ \cCol ->
    withHs outlineCol $ \cOutlineCol ->
      c_Shape_CreateRectangle (realToFrac p1X) (realToFrac p1Y) (realToFrac p2X) (realToFrac p2Y) cCol (realToFrac outline) cOutlineCol >>= createFromC

createCircle :: (Float, Float) -> Float -> Color -> Float -> Color -> IO Shape
createCircle (x, y) radius col outline outlineCol =
  withHs col $ \cCol ->
    withHs outlineCol $ \cOutlineCol ->
      c_Shape_CreateCircle (realToFrac x) (realToFrac y) (realToFrac radius) cCol (realToFrac outline) cOutlineCol >>= createFromC

addPoint :: Shape -> Float -> Float -> Color -> Color -> IO ()
addPoint shape x y col outlineCol =
  withHs shape $ \shapePtr ->
    withHs col $ \cCol ->
      withHs outlineCol $ \cOutlineCol ->
        c_Shape_AddPoint shapePtr (realToFrac x)
                         (realToFrac y) cCol cOutlineCol

enableFill :: Shape -> Bool -> IO ()
enableFill shape b =
  withHs shape $ \shapePtr ->
    c_Shape_EnableFill shapePtr b

enableOutline :: Shape -> Bool -> IO ()
enableOutline shape b =
  withHs shape $ \shapePtr ->
    c_Shape_EnableOutline shapePtr b

setOutlineWidth :: Shape -> Float -> IO ()
setOutlineWidth shape width =
  withHs shape $ \shapePtr ->
    c_Shape_SetOutlineWidth shapePtr (realToFrac width)

getOutlineWidth :: Shape -> IO Float
getOutlineWidth shape =
  withHs shape $ \shapePtr ->
    realToFrac `fmap` c_Shape_GetOutlineWidth shapePtr

getNbPoints :: Shape -> IO Int
getNbPoints shape =
  withHs shape $ \shapePtr ->
    fromIntegral `fmap` c_Shape_GetNbPoints shapePtr

getPointPosition :: Shape -> Int -> IO (Float, Float)
getPointPosition shape index =
  withHs shape $ \shapePtr ->
    with 0 $ \xPtr ->
      with 0 $ \yPtr -> do
        c_Shape_GetPointPosition shapePtr (fromIntegral index) xPtr yPtr
        liftM2 (,) (realToFrac `fmap` peek xPtr)
                   (realToFrac `fmap` peek yPtr)

getPointColor :: Shape -> Int -> IO Color
getPointColor shape index =
  withHs shape $ \ptr ->
    withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
      c_Shape_GetPointColor ptr (fromIntegral index) clrPtr
      createFromC clrPtr

getPointOutlineColor :: Shape -> Int -> IO Color
getPointOutlineColor shape index =
  withHs shape $ \ptr ->
    withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
      c_Shape_GetPointOutlineColor ptr (fromIntegral index) clrPtr
      createFromC clrPtr

setPointPosition :: Shape -> Int -> (Float, Float) -> IO ()
setPointPosition shape index (x, y) =
  withHs shape $ \ptr ->
    c_Shape_SetPointPosition ptr (fromIntegral index) (realToFrac x) (realToFrac y)

setPointColor :: Shape -> Int -> Color -> IO ()
setPointColor shape index color =
  withHs shape $ \ptr ->
    withHs color $ \cColor ->
      c_Shape_SetPointColor ptr (fromIntegral index) cColor

setPointOutlineColor :: Shape -> Int -> Color -> IO ()
setPointOutlineColor shape index color =
  withHs shape $ \ptr ->
    withHs color $ \cColor ->
      c_Shape_SetPointOutlineColor ptr (fromIntegral index) cColor

