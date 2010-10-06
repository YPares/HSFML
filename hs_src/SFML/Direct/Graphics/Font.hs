{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Direct.Graphics.Font where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified Data.ByteString as BS

import SFML.Direct.CImport.Font
import SFML.Direct.Types.Font
import SFML.Direct.Types.SfInts
import SFML.Direct.Classes.Convertible_HS_C
import SFML.Utils


createFont :: IO Font
createFont = c_Font_Create >>= createFromC

{-createFontFromFile :: FilePath -> Int -> Maybe String -> IO Font
createFontFromFile fp charSize maybeCharset =
  withCString fp $ \fpPtr ->
    maybeWith withArray maybeList $ \uint32Ptr ->
      c_Font_CreateFromFile fpPtr (fromIntegral charSize) uint32Ptr >>=
      createFromC
  where maybeList = maybeCharset >>= return . byteStringToSfUint32List .
                                     toUTF32ByteString
-}

{-createFontFromMemory :: BS.ByteString -> Int -> Maybe String -> IO Font
createFontFromMemory fontData charSize maybeCharset =
  BS.useAsCStringLen fontData $ \(dataPtr, len) ->
    maybeWith withArray maybeList $ \uint32Ptr ->
      c_Font_CreateFromMemory dataPtr (fromIntegral len)
                              (fromIntegral charSize) uint32Ptr >>=
      createFromC
  where maybeList = maybeCharset >>= return . byteStringToSfUint32List .
                                     toUTF32ByteString
-}

getCharacterSize :: Font -> IO Int
getCharacterSize font =
  withHs font $ \fontPtr ->
    fromIntegral `fmap` c_Font_GetCharacterSize fontPtr

getDefaultFont :: IO Font
getDefaultFont = c_Font_GetDefaultFont >>= createFromC_

