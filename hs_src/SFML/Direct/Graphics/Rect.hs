module SFML.Direct.Graphics.Rect where

import Foreign
import Foreign.C.Types

import SFML.Direct.CImport.Rect
import SFML.Direct.Types.Rect
import SFML.Direct.Classes.Convertible_HS_C


class RectCoord t where
  offset :: Rect t -> t -> t -> Rect t
  contains :: Rect t -> t -> t -> Bool
  intersects :: Rect t -> Rect t -> Maybe (Rect t)

instance RectCoord Int where
  offset rect x y = unsafePerformIO $ 
    withHs rect $ \rectPtr -> do
      c_IntRect_Offset rectPtr (fromIntegral x) (fromIntegral y)
      createFromC rectPtr

  contains rect x y = unsafePerformIO $
    withHs rect $ \rectPtr -> do
      c_IntRect_Contains rectPtr (fromIntegral x) (fromIntegral y)

  intersects rect1 rect2 = unsafePerformIO $ do
    withHs rect1 $ \rectPtr1 ->
      withHs rect2 $ \rectPtr2 ->
        withHs (Rect 0 0 0 0 :: Rect Int) $ \resPtr -> do
          intersection <- c_IntRect_Intersects rectPtr1 rectPtr2 resPtr
          if intersection
            then Just `fmap` createFromC resPtr
            else return Nothing

instance RectCoord Float where
  offset rect x y = unsafePerformIO $ 
    withHs rect $ \rectPtr -> do
      c_FloatRect_Offset rectPtr (realToFrac x) (realToFrac y)
      createFromC rectPtr

  contains rect x y = unsafePerformIO $
    withHs rect $ \rectPtr -> do
      c_FloatRect_Contains rectPtr (realToFrac x) (realToFrac y)

  intersects rect1 rect2 = unsafePerformIO $ do
    withHs rect1 $ \rectPtr1 ->
      withHs rect2 $ \rectPtr2 ->
        withHs (Rect 0 0 0 0 :: Rect Float) $ \resPtr -> do
          intersection <- c_FloatRect_Intersects rectPtr1 rectPtr2 resPtr
          if intersection
            then Just `fmap` createFromC resPtr
            else return Nothing

