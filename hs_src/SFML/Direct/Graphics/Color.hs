module SFML.Direct.Graphics.Color where

import Foreign

import SFML.Direct.CImport.Color 
import SFML.Direct.Types.Color 
import SFML.Direct.Classes.Convertible_HS_C


fromRGB :: ColorCoord -> ColorCoord -> ColorCoord -> Color
fromRGB r g b = unsafePerformIO $
  withHs (Color 0 0 0 0 :: Color) $ \resPtr -> do
    c_Color_FromRGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
                    resPtr
    createFromC resPtr

fromRGBA :: ColorCoord -> ColorCoord -> ColorCoord -> ColorCoord -> Color
fromRGBA r g b a = unsafePerformIO $
  withHs (Color 0 0 0 0 :: Color) $ \resPtr -> do
    c_Color_FromRGBA (fromIntegral r) (fromIntegral g) (fromIntegral b)
                     (fromIntegral a) resPtr
    createFromC resPtr

add :: Color -> Color -> Color
add cl1 cl2 = unsafePerformIO $
  withHs cl1 $ \cl1Ptr ->
    withHs cl2 $ \cl2Ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \resPtr -> do
        c_Color_Add cl1Ptr cl2Ptr resPtr
        createFromC resPtr

modulate :: Color -> Color -> Color
modulate cl1 cl2 = unsafePerformIO $
  withHs cl1 $ \cl1Ptr ->
    withHs cl2 $ \cl2Ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \resPtr -> do
        c_Color_Modulate cl1Ptr cl2Ptr resPtr
        createFromC resPtr

