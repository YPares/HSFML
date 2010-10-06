module SFML.Direct.Graphics.PostFX where

import Foreign.C.String

import SFML.Direct.CImport.PostFX
import SFML.Direct.Types.PostFX
import SFML.Direct.Types.Image
import SFML.Direct.Classes.Convertible_HS_C


createPostFXFromFile :: FilePath -> IO PostFX
createPostFXFromFile fname =
  withCString fname $ \cstr ->
    c_PostFX_CreateFromFile cstr >>= createFromC

createPostFXFromMemory :: String -> IO PostFX
createPostFXFromMemory effect =
  withCString effect $ \cstr ->
    c_PostFX_CreateFromMemory cstr >>= createFromC

setParameter1 :: PostFX -> String -> Float -> IO ()
setParameter1 fx param val =
  withHs fx $ \fxPtr ->
    withCString param $ \cstr ->
      c_PostFX_SetParameter1 fxPtr cstr (realToFrac val)

setParameter2 :: PostFX -> String -> Float -> Float -> IO ()
setParameter2 fx param val1 val2 =
  withHs fx $ \fxPtr ->
    withCString param $ \cstr ->
      c_PostFX_SetParameter2 fxPtr cstr (realToFrac val1) (realToFrac val2)

setParameter3 :: PostFX -> String -> Float -> Float -> Float -> IO ()
setParameter3 fx param val1 val2 val3 =
  withHs fx $ \fxPtr ->
    withCString param $ \cstr ->
      c_PostFX_SetParameter3 fxPtr cstr (realToFrac val1) (realToFrac val2) (realToFrac val3)

setParameter4 :: PostFX -> String -> Float -> Float -> Float -> Float -> IO ()
setParameter4 fx param val1 val2 val3 val4 =
  withHs fx $ \fxPtr ->
    withCString param $ \cstr ->
      c_PostFX_SetParameter4 fxPtr cstr (realToFrac val1) (realToFrac val2) (realToFrac val3) (realToFrac val4)

setTexture :: PostFX -> String -> Image -> IO ()
setTexture fx str img =
  withHs fx $ \fxPtr ->
    withCString str $ \strPtr ->
      withHs img $ \imgPtr ->
        c_PostFX_SetTexture fxPtr strPtr imgPtr

canUsePostFX :: IO Bool
canUsePostFX = c_PostFX_CanUsePostFX

