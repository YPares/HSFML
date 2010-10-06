{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

module SFML.Direct.Classes.Convertible_HS_C where

import Foreign
import Control.Monad (liftM)


class (Monad m) => Convertible_HS_C hs c m | hs -> c where
  withHs :: hs -> (c -> m a) -> m a
  createFromC :: c -> m hs
  {- | A special version of createFromC which doesn't attach
       finalizer to the data -}
  createFromC_ :: c -> m hs
  createFromC_ = createFromC

maybeCreateFromC :: (Convertible_HS_C hs (Ptr a) m) =>
                    Ptr a -> m (Maybe hs)
maybeCreateFromC x
  | x == nullPtr = return Nothing
  | otherwise    = Just `liftM` createFromC x

maybeCreateFromC_ :: (Convertible_HS_C hs (Ptr a) m) =>
                     Ptr a -> m (Maybe hs)
maybeCreateFromC_ x
  | x == nullPtr = return Nothing
  | otherwise    = Just `liftM` createFromC_ x

