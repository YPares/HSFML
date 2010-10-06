{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.PostFX where

import Foreign

import SFML.Direct.Types.Image
import SFML.Direct.Classes.Convertible_HS_C


data AbsPostFX

foreign import ccall unsafe "&sfPostFX_Destroy"
  c_PostFX_Destroy :: FunPtr (Ptr AbsPostFX -> IO ())

data PostFX = PostFX !(ForeignPtr AbsPostFX)

instance Convertible_HS_C PostFX (Ptr AbsPostFX) IO where
  withHs (PostFX fxFp) f = withForeignPtr fxFp f
  createFromC ptr = PostFX `fmap` newForeignPtr c_PostFX_Destroy ptr
  createFromC_ ptr = PostFX `fmap` newForeignPtr_ ptr

