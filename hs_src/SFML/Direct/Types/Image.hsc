{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Direct.Types.Image where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsImage

foreign import ccall unsafe "&sfImage_Destroy"
  c_Image_Destroy :: FunPtr (Ptr AbsImage -> IO ())

data Image = Image !(ForeignPtr AbsImage)
  deriving (Show)

instance Convertible_HS_C Image (Ptr AbsImage) IO where
  withHs (Image img) f = withForeignPtr img f
  createFromC ptr = Image `fmap` newForeignPtr c_Image_Destroy ptr
  createFromC_ ptr = Image `fmap` newForeignPtr_ ptr

