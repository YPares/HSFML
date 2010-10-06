{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Sprite where

import Foreign
import Data.IORef
import Control.Monad (liftM2)

import SFML.Direct.Types.Image
import SFML.Direct.Classes.Convertible_HS_C


data AbsSprite

foreign import ccall unsafe "&sfSprite_Destroy"
  c_Sprite_Destroy :: FunPtr (Ptr AbsSprite -> IO ())

data Sprite = Sprite !(ForeignPtr AbsSprite) !(IORef (Maybe Image))
                                             -- A sprite keeps a reference
                                             -- to its image so that the
                                             -- latter is not garbage
                                             -- collected

instance Convertible_HS_C Sprite (Ptr AbsSprite) IO where
  withHs (Sprite sprFp _) f = withForeignPtr sprFp f
  createFromC ptr = liftM2 Sprite (newForeignPtr c_Sprite_Destroy ptr)
                                  (newIORef Nothing)
  createFromC_ ptr = liftM2 Sprite (newForeignPtr_ ptr)
                                   (newIORef Nothing)

