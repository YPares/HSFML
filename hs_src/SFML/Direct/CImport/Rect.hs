{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Rect where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Rect


foreign import ccall unsafe "sfFloatRect_Offset"
  c_FloatRect_Offset :: Ptr (Rect CFloat) -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "sfIntRect_Offset"
  c_IntRect_Offset :: Ptr (Rect CInt) -> CInt -> CInt -> IO ()

foreign import ccall unsafe "sfFloatRect_Contains"
  c_FloatRect_Contains :: Ptr (Rect CFloat) -> CFloat -> CFloat -> IO Bool

foreign import ccall unsafe "sfIntRect_Contains"
  c_IntRect_Contains :: Ptr (Rect CInt) -> CInt -> CInt -> IO Bool

foreign import ccall unsafe "sfFloatRect_Intersects"
  c_FloatRect_Intersects :: Ptr (Rect CFloat) -> Ptr (Rect CFloat) -> 
                            Ptr (Rect CFloat) -> IO Bool

foreign import ccall unsafe "sfIntRect_Intersects"
  c_IntRect_Intersects :: Ptr (Rect CInt) -> Ptr (Rect CInt) -> 
                          Ptr (Rect CInt) -> IO Bool
