{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module SFML.Direct.Classes.Handling where

import SFML.Direct.Types.Color
import SFML.Direct.Types.Rect


class (Num dim, Monad m) => Sized obj dim m | obj -> dim where
  getSize :: obj -> m (dim, dim)

class (Sized obj dim m) => Resizable obj dim m | obj -> dim where
  setSize :: obj -> (dim, dim) -> m ()

class (Num dim, Monad m) => Positionable obj dim m | obj -> dim where
  setPosition :: obj -> (dim, dim) -> m ()

class (Num dim, Monad m) => Movable obj dim m | obj -> dim where
  move :: obj -> (dim, dim) -> m ()

class (Num dim, Monad m) => HasCenter obj dim m | obj -> dim where
  getCenter :: obj -> m (dim, dim)
  setCenter :: obj -> (dim, dim) -> m ()

class (Monad m) => Rasterized obj m where
  getPixel :: obj -> (Int, Int) -> m Color

class (Monad m) => BoundedByRect obj rectType m where
  getRect :: obj -> m (Rect rectType)

