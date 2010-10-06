{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.RenderWindow where

import Foreign
import Control.Monad (liftM2)

import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.CImport.Event
import SFML.Direct.Types.Event


data AbsRenderWindow

foreign import ccall unsafe "&sfRenderWindow_Destroy"
  c_RenderWindow_Destroy :: FunPtr (Ptr AbsRenderWindow -> IO ())

data RenderWindow = RenderWindow !(ForeignPtr AbsRenderWindow) !(ForeignPtr AbsEvent)
  deriving (Show)

instance Convertible_HS_C RenderWindow (Ptr AbsRenderWindow) IO where
  withHs (RenderWindow rw _) f = withForeignPtr rw f
  createFromC ptr = liftM2 RenderWindow
                     (newForeignPtr c_RenderWindow_Destroy ptr) 
                     (c_Event_Create >>= newForeignPtr finalizerFree)
  createFromC_ ptr = liftM2 RenderWindow
                     (newForeignPtr_ ptr) 
                     (c_Event_Create >>= newForeignPtr finalizerFree)

