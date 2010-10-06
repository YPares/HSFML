{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Sleep where

import Foreign.C.Types


foreign import ccall unsafe "sfSleep"
  c_Sleep :: CFloat -> IO ()

