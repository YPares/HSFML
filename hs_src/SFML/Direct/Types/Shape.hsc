{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Shape where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsShape

foreign import ccall unsafe "&sfShape_Destroy"
  c_Shape_Destroy :: FunPtr (Ptr AbsShape -> IO ())

data Shape = Shape !(ForeignPtr AbsShape)
  deriving (Show)

instance Convertible_HS_C Shape (Ptr AbsShape) IO where
  withHs (Shape v) f = withForeignPtr v f
  createFromC ptr = Shape `fmap` newForeignPtr c_Shape_Destroy ptr
  createFromC_ ptr = Shape `fmap` newForeignPtr_ ptr

