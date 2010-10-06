{-# LANGUAGE MultiParamTypeClasses #-}

module SFML.Direct.Graphics.Image where

import Foreign.C.String
import Control.Monad (liftM2)

import SFML.Direct.CImport.Image
import SFML.Direct.Types.Image
import SFML.Direct.Types.Rect
import SFML.Direct.Types.RenderWindow
import SFML.Direct.Types.Color
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling


instance Sized Image Int IO where
  getSize img = withHs img $ \imgPtr ->
    let get f = fromIntegral `fmap` f imgPtr
    in liftM2 (,) (get c_Image_GetWidth) (get c_Image_GetHeight) 

instance Rasterized Image IO where
  getPixel img (x, y) = do
    withHs img $ \imgPtr ->
      withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
        c_Image_GetPixel imgPtr (fromIntegral x) (fromIntegral y) clrPtr
        createFromC clrPtr

createImage :: IO (Maybe Image)
createImage = c_Image_Create >>= maybeCreateFromC

createImageFromColor :: Int -> Int -> Color -> IO (Maybe Image)
createImageFromColor width height clr =
  withHs clr $ \clrPtr ->
    c_Image_CreateFromColor (fromIntegral width)
                            (fromIntegral height) clrPtr >>=
      maybeCreateFromC

--createFromPixels

createImageFromFile :: FilePath -> IO (Maybe Image)
createImageFromFile fname = withCString fname $ \fnameCStr -> do
  c_Image_CreateFromFile fnameCStr >>= maybeCreateFromC

--createFromMemory

saveToFile :: Image -> FilePath -> IO ()
saveToFile img fname =
  withHs img $ \imgPtr ->
    withCString fname $ \fnameCStr ->
      c_Image_SaveToFile imgPtr fnameCStr

createMaskFromColor :: Image -> Color -> Int -> IO ()
createMaskFromColor img clr alpha =
  withHs img $ \imgPtr ->
    withHs clr $ \clrPtr ->
      c_Image_CreateMaskFromColor imgPtr clrPtr (fromIntegral alpha)

copy :: Image -> Image -> Int -> Int -> Rect Int -> IO ()
copy img src destX destY srcRect =
  withHs img $ \imgPtr ->
    withHs src $ \srcPtr ->
      withHs srcRect $ \srcRectPtr ->
        c_Image_Copy imgPtr srcPtr (fromIntegral destX)
                     (fromIntegral destY) srcRectPtr

copyScreen :: Image -> RenderWindow -> Rect Int -> IO Bool
copyScreen img win srcRect =
  withHs img $ \imgPtr ->
    withHs win $ \winPtr ->
      withHs srcRect $ \srcRectPtr ->
        c_Image_CopyScreen imgPtr winPtr srcRectPtr

setPixel :: Image -> (Int, Int) -> Color -> IO ()
setPixel img (x, y) clr =
  withHs img $ \imgPtr ->
    withHs clr $ \clrPtr ->
      c_Image_SetPixel imgPtr (fromIntegral x) (fromIntegral y) clrPtr

--getPixelsPtr

bind :: Image -> IO ()
bind img = withHs img $ \imgPtr ->
  c_Image_Bind imgPtr

setSmooth :: Image -> Bool -> IO ()
setSmooth img b = withHs img $ \imgPtr ->
  c_Image_SetSmooth imgPtr b

isSmooth :: Image -> IO Bool
isSmooth img = withHs img $ \imgPtr ->
  c_Image_IsSmooth imgPtr

