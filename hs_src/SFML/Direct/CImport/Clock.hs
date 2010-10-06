{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Clock where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Clock


foreign import ccall unsafe "sfClock_Create"
  c_Clock_Create :: IO (Ptr AbsClock)

foreign import ccall unsafe "sfClock_GetTime"
  c_Clock_GetTime :: Ptr AbsClock -> IO CFloat

foreign import ccall unsafe "sfClock_Reset"
  c_Clock_Reset :: Ptr AbsClock -> IO ()

