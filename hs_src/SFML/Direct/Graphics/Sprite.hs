{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Direct.Graphics.Sprite where

import Foreign
import Foreign.C.Types
import Data.IORef
import Control.Monad (liftM2)

import SFML.Direct.CImport.Sprite
import SFML.Direct.CImport.RenderWindow
import SFML.Direct.Types.Sprite
import SFML.Direct.Types.Image
import SFML.Direct.Types.Color
import SFML.Direct.Types.Rect
import SFML.Direct.Types.Enums
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling
import SFML.Direct.Classes.Drawable


instance Sized Sprite Float IO where
  getSize sprite =
    withHs sprite $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Sprite_GetWidth) (get c_Sprite_GetHeight)

instance Resizable Sprite Float IO where
  setSize sprite (width, height) =
    withHs sprite $ \ptr ->
      c_Sprite_Resize ptr (realToFrac width) (realToFrac height)

instance Positionable Sprite Float IO where
  setPosition sprite (x, y) =
    withHs sprite $ \ptr ->
      c_Sprite_SetPosition ptr (realToFrac x) (realToFrac y)

instance HasCenter Sprite Float IO where
  setCenter sprite (x, y) =
    withHs sprite $ \ptr ->
      c_Sprite_SetCenter ptr (realToFrac x) (realToFrac y)

  getCenter sprite =
    withHs sprite $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Sprite_GetCenterX) (get c_Sprite_GetCenterY)

instance Movable Sprite Float IO where
  move sprite (x, y) =
    withHs sprite $ \ptr ->
      c_Sprite_Move ptr (realToFrac x) (realToFrac y)

instance Rasterized Sprite IO where
  getPixel sprite (x, y) =
    withHs sprite $ \ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
        c_Sprite_GetPixel ptr (fromIntegral x) (fromIntegral y) clrPtr
        createFromC clrPtr

instance Drawable Sprite Float IO where
  setX sprite x =
    withHs sprite $ \ptr ->
      c_Sprite_SetX ptr (realToFrac x)

  setY sprite y =
    withHs sprite $ \ptr ->
      c_Sprite_SetY ptr (realToFrac y)

  setScaleX sprite x =
    withHs sprite $ \ptr ->
      c_Sprite_SetScaleX ptr (realToFrac x)

  setScaleY sprite y =
    withHs sprite $ \ptr ->
      c_Sprite_SetScaleY ptr (realToFrac y)

  setScale sprite (x, y) =
    withHs sprite $ \ptr ->
      c_Sprite_SetScale ptr (realToFrac x) (realToFrac y)

  setRotation sprite rot =
    withHs sprite $ \ptr ->
      c_Sprite_SetRotation ptr (realToFrac rot)

  setColor sprite color =
    withHs sprite $ \ptr ->
      withHs color $ \clrPtr ->
        c_Sprite_SetColor ptr clrPtr
   
  setBlendMode sprite bm =
    withHs sprite $ \ptr ->
      withHs bm $ \cBM ->
        c_Sprite_SetBlendMode ptr cBM

  getPosition sprite =
    withHs sprite $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Sprite_GetX) (get c_Sprite_GetY)

  getScale sprite =
    withHs sprite $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Sprite_GetScaleX) (get c_Sprite_GetScaleY)

  getRotation sprite =
    withHs sprite $ \ptr ->
      realToFrac `fmap` c_Sprite_GetRotation ptr

  getColor sprite =
    withHs sprite $ \ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
        c_Sprite_GetColor ptr clrPtr
        createFromC clrPtr

  getBlendMode sprite =
    withHs sprite $ \ptr ->
      c_Sprite_GetBlendMode ptr >>= createFromC
      
  scale sprite (x, y) =
    withHs sprite $ \ptr ->
      c_Sprite_Scale ptr (realToFrac x) (realToFrac y)

  rotate sprite angle =
    withHs sprite $ \ptr ->
      c_Sprite_Rotate ptr (realToFrac angle)

  transformToLocal sprite (ptX, ptY) =
    withHs sprite $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Sprite_TransformToLocal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)

  transformToGlobal sprite (ptX, ptY) =
    withHs sprite $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Sprite_TransformToGlobal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)
 
  drawFunc sprite wPtr =
    withHs sprite $ \spritePtr ->
      c_RenderWindow_DrawSprite wPtr spritePtr

createSprite :: IO Sprite
createSprite = c_Sprite_Create >>= createFromC

createSpriteFromImage :: Image -> IO Sprite
createSpriteFromImage img = do
  spr <- createSprite
  setImage spr img
  return spr

setImage :: Sprite -> Image -> IO ()
setImage sprite@(Sprite _ imgRef) img = do
  writeIORef imgRef $ Just img
  withHs sprite $ \ptr ->
    withHs img $ \imgPtr -> do
      c_Sprite_SetImage ptr imgPtr

setSubRect :: Sprite -> Rect Int -> IO ()
setSubRect sprite rect =
  withHs sprite $ \ptr ->
    withHs rect $ \rectPtr ->
      c_Sprite_SetSubRect ptr rectPtr

flipX :: Sprite -> Bool -> IO ()
flipX sprite b =
  withHs sprite $ \ptr ->
    c_Sprite_FlipX ptr b

flipY :: Sprite -> Bool -> IO ()
flipY sprite b =
  withHs sprite $ \ptr ->
    c_Sprite_FlipY ptr b

getImage :: Sprite -> IO (Maybe Image)
getImage sprite =
  withHs sprite $ \sprPtr -> do
    c_Sprite_GetImage sprPtr >>= maybeCreateFromC_

getSubRect :: Sprite -> IO (Rect Int)
getSubRect sprite =
  withHs sprite $ \ptr ->
    withHs (Rect 0 0 0 0 :: Rect Int) $ \rectPtr -> do
      c_Sprite_GetSubRect ptr rectPtr
      createFromC rectPtr

