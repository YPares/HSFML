{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Direct.Graphics.Text where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (liftM2)
import Control.Monad.Identity
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map as M

import SFML.Direct.CImport.Text
import SFML.Direct.CImport.RenderWindow
import SFML.Direct.Types.Text
import SFML.Direct.Types.Color
import SFML.Direct.Types.Rect
import SFML.Direct.Types.Enums
import SFML.Direct.Types.Font
import SFML.Direct.Types.SfInts
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling
import SFML.Direct.Classes.Drawable
import SFML.Utils


instance Positionable Text Float IO where
  setPosition text (x, y) =
    withHs text $ \ptr ->
      c_Text_SetPosition ptr (realToFrac x) (realToFrac y)

instance HasCenter Text Float IO where
  setCenter text (x, y) =
    withHs text $ \ptr ->
      c_Text_SetCenter ptr (realToFrac x) (realToFrac y)

  getCenter text =
    withHs text $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Text_GetCenterX) (get c_Text_GetCenterY)

instance Movable Text Float IO where
  move text (x, y) =
    withHs text $ \ptr ->
      c_Text_Move ptr (realToFrac x) (realToFrac y)

instance Drawable Text Float IO where
  setX text x =
    withHs text $ \ptr ->
      c_Text_SetX ptr (realToFrac x)

  setY text y =
    withHs text $ \ptr ->
      c_Text_SetY ptr (realToFrac y)

  setScaleX text x =
    withHs text $ \ptr ->
      c_Text_SetScaleX ptr (realToFrac x)

  setScaleY text y =
    withHs text $ \ptr ->
      c_Text_SetScaleY ptr (realToFrac y)

  setScale text (x, y) =
    withHs text $ \ptr ->
      c_Text_SetScale ptr (realToFrac x) (realToFrac y)

  setRotation text rot =
    withHs text $ \ptr ->
      c_Text_SetRotation ptr (realToFrac rot)

  setColor text color =
    withHs text $ \ptr ->
      withHs color $ \clrPtr ->
        c_Text_SetColor ptr clrPtr
   
  setBlendMode text bm =
    withHs text $ \ptr ->
      withHs bm $ \cBM ->
        c_Text_SetBlendMode ptr cBM

  getPosition text =
    withHs text $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Text_GetX) (get c_Text_GetY)

  getScale text =
    withHs text $ \ptr ->
      let get f = realToFrac `fmap` f ptr
      in liftM2 (,) (get c_Text_GetScaleX) (get c_Text_GetScaleY)

  getRotation text =
    withHs text $ \ptr ->
      realToFrac `fmap` c_Text_GetRotation ptr

  getColor text =
    withHs text $ \ptr ->
      withHs (Color 0 0 0 0 :: Color) $ \clrPtr -> do
        c_Text_GetColor ptr clrPtr
        createFromC clrPtr

  getBlendMode text =
    withHs text $ \ptr ->
      c_Text_GetBlendMode ptr >>= createFromC
      
  scale text (x, y) =
    withHs text $ \ptr ->
      c_Text_Scale ptr (realToFrac x) (realToFrac y)

  rotate text angle =
    withHs text $ \ptr ->
      c_Text_Rotate ptr (realToFrac angle)

  transformToLocal text (ptX, ptY) =
    withHs text $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Text_TransformToLocal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)

  transformToGlobal text (ptX, ptY) =
    withHs text $ \ptr ->
      with (0::CFloat) $ \x ->
        with (0::CFloat) $ \y -> do
          c_Text_TransformToGlobal ptr (realToFrac ptX) (realToFrac ptY) x y
          liftM2 (,) (realToFrac `fmap` peek x) (realToFrac `fmap` peek y)

  drawFunc text wPtr = 
    withHs text $ \textPtr ->
      c_RenderWindow_DrawText wPtr textPtr


createText :: IO Text
createText = c_Text_Create >>= createFromC

setString :: Text -> String -> IO ()
setString txt str =
  withHs txt $ \txtPtr ->
    BS.useAsCString (toUTF8ByteString str) $ \strPtr ->
      c_Text_SetString txtPtr strPtr

{-setUnicodeString :: Text -> String -> IO ()
setUnicodeString txt str =
  withHs txt $ \txtPtr ->
    withArray (byteStringToSfUint32List $ toUTF32ByteString str) $
      \uint32Ptr -> c_Text_SetUnicodeString txtPtr uint32Ptr
-}

setFont :: Text -> Font -> IO ()
setFont txt@(Text _ fontRef) font = do
  writeIORef fontRef $ Just font
  withHs txt $ \txtPtr ->
    withHs font $ \fontPtr ->
      c_Text_SetFont txtPtr fontPtr

setTextSize :: Text -> Float -> IO ()
setTextSize txt size =
  withHs txt $ \txtPtr ->
    c_Text_SetSize txtPtr $ realToFrac size

setStyle :: Text -> [TextStyle] -> IO ()
setStyle txt styles =
  withHs txt $ \txtPtr ->
    c_Text_SetStyle txtPtr convStyles
  where convStyles | null styles = textRegular
                   | otherwise   = foldr (.|.) 0 styleVals
          where styleVals = flip map styles $ \s ->
                              runIdentity $ withHs s return

{-getUnicodeString :: Text -> IO String
getUnicodeString txt =
  withHs txt $ \txtPtr ->
    c_Text_GetUnicodeString txtPtr >>= peekArray0 0 >>=
    return . fromUTF32ByteString . sfUint32ListToByteString
-}

getString :: Text -> IO String
getString txt =
  withHs txt $ \txtPtr ->
    fromUTF8ByteString `fmap` (c_Text_GetString txtPtr >>= BS.packCString)

getFont :: Text -> IO Font
getFont txt = 
  withHs txt $ \txtPtr ->
    c_Text_GetFont txtPtr >>= createFromC_

getTextSize :: Text -> IO Float
getTextSize txt =
  withHs txt $ \txtPtr ->
    realToFrac `fmap` c_Text_GetSize txtPtr

getStyle :: Text -> IO [TextStyle]
getStyle txt =
  withHs txt $ \txtPtr -> do
    c_styles <- c_Text_GetStyle txtPtr
    let hs_styles = foldr (\(val, dt) acc ->
                            if c_styles .&. val /= 0
                              then dt:acc
                              else acc)
                          [] (M.toList textStyleMapping)
    return $ if null hs_styles then [TextRegular] else hs_styles

getCharacterPos :: Text -> Int -> IO (Float, Float)
getCharacterPos txt index =
  withHs txt $ \txtPtr ->
    with 0 $ \xPtr ->
      with 0 $ \yPtr -> do
        c_Text_GetCharacterPos txtPtr (fromIntegral index) xPtr yPtr
        liftM2 (,) (realToFrac `fmap` peek xPtr)
                   (realToFrac `fmap` peek yPtr)

instance BoundedByRect Text Float IO where
  getRect txt =
    withHs txt $ \txtPtr ->
      withHs (Rect 0 0 0 0 :: Rect Float) $ \rectPtr -> do
        c_Text_GetRect txtPtr rectPtr
        createFromC rectPtr

