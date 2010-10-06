{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.View where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsView

foreign import ccall unsafe "&sfView_Destroy"
  c_View_Destroy :: FunPtr (Ptr AbsView -> IO ())

data View = View !(ForeignPtr AbsView)
  deriving (Show)

instance Convertible_HS_C View (Ptr AbsView) IO where
  withHs (View v) f = withForeignPtr v f
  createFromC ptr = View `fmap` newForeignPtr c_View_Destroy ptr
  createFromC_ ptr = View `fmap` newForeignPtr_ ptr

