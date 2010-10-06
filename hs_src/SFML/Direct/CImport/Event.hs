{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Event where

import Foreign
import Foreign.C.Types

import SFML.Direct.Types.Event


foreign import ccall unsafe "sfEvent_Create"
  c_Event_Create :: IO (Ptr AbsEvent)

