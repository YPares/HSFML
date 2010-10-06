{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Clock where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsClock

foreign import ccall unsafe "&sfClock_Destroy"
  c_Clock_Destroy :: FunPtr (Ptr AbsClock -> IO ())


data Clock = Clock !(ForeignPtr AbsClock)
  deriving (Show)

instance Convertible_HS_C Clock (Ptr AbsClock) IO where
  withHs (Clock clk) f = withForeignPtr clk f
  createFromC cPtr = Clock `fmap` newForeignPtr c_Clock_Destroy cPtr
  createFromC_ cPtr = Clock `fmap` newForeignPtr_ cPtr

