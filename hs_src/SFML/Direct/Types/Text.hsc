{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Text where

import Foreign
import Data.IORef
import Control.Monad (liftM2)

import SFML.Direct.Types.Font
import SFML.Direct.Classes.Convertible_HS_C


data AbsText

foreign import ccall unsafe "&sfString_Destroy"
  c_Text_Destroy :: FunPtr (Ptr AbsText -> IO ())

data Text = Text !(ForeignPtr AbsText) !(IORef (Maybe Font))

instance Convertible_HS_C Text (Ptr AbsText) IO where
  withHs (Text v _) f = withForeignPtr v f
  createFromC ptr = liftM2 Text (newForeignPtr c_Text_Destroy ptr)
                                (newIORef Nothing)
  createFromC_ ptr = liftM2 Text (newForeignPtr_ ptr)
                                 (newIORef Nothing)

