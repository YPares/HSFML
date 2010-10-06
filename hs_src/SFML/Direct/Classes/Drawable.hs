{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances, TypeSynonymInstances #-}

module SFML.Direct.Classes.Drawable where

import Foreign
import Data.Sequence

import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Classes.Handling
import SFML.Direct.Types.Color
import SFML.Direct.Types.Enums
import SFML.Direct.Types.RenderWindow


class (Positionable obj dim m, HasCenter obj dim m, Movable obj dim m) =>
      Drawable obj dim m where
  setX :: obj -> dim -> m ()
  setY :: obj -> dim -> m ()
  setScaleX :: obj -> dim -> m ()
  setScaleY :: obj -> dim -> m ()
  setScale :: obj -> (dim, dim) -> m ()
  setRotation :: obj -> dim -> m ()
  setColor :: obj -> Color -> m ()
  setBlendMode :: obj -> BlendMode -> m ()
  getPosition :: obj -> m (dim, dim)
  getScale :: obj -> m (dim, dim)
  getRotation :: obj -> m dim
  getColor :: obj -> m Color
  getBlendMode :: obj -> m BlendMode
  scale :: obj -> (dim, dim) -> m ()
  rotate :: obj -> dim -> m ()
  transformToLocal :: obj -> (dim, dim) -> m (dim, dim)
  transformToGlobal :: obj -> (dim, dim) -> m (dim, dim)
  drawFunc :: obj -> Ptr AbsRenderWindow -> m ()


data DrawableEDT dim m = forall obj. (Drawable obj dim m) =>
                         DrawableEDT obj

type DrawableSeq dim m = Seq (DrawableEDT dim m)

(||>) :: (Monad m, Drawable obj dim m) =>
         DrawableSeq dim m -> obj -> DrawableSeq dim m
ds ||> obj = ds |> DrawableEDT obj
infixl 5 ||>

(<||) :: (Monad m, Drawable obj dim m) =>
         obj -> DrawableSeq dim m -> DrawableSeq dim m
(<||) = flip (||>)
infixl 5 <||

