{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Input where

import Foreign

import SFML.Direct.Classes.Convertible_HS_C


data AbsInput 

newtype Input = Input (Ptr AbsInput)
  deriving (Show)

instance (Monad m) => Convertible_HS_C Input Input m where
  withHs inp f = f inp
  createFromC inp = return inp

