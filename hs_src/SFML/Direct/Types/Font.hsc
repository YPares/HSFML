{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Font where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsFont

foreign import ccall unsafe "&sfFont_Destroy"
  c_Font_Destroy :: FunPtr (Ptr AbsFont -> IO ())

data Font = Font !(ForeignPtr AbsFont)
  deriving (Show)

instance Convertible_HS_C Font (Ptr AbsFont) IO where
  withHs (Font v) f = withForeignPtr v f
  createFromC ptr = Font `fmap` newForeignPtr c_Font_Destroy ptr
  createFromC_ ptr = Font `fmap` newForeignPtr_ ptr

