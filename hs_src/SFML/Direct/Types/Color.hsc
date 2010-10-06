{-# LANGUAGE EmptyDataDecls, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Color where

import Foreign
import Foreign.C.Types

import SFML.Direct.Classes.Convertible_HS_C
import SFML.Direct.Types.SfInts

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include <Wrappers.h>


data ColorType a = Color { red :: a, green :: a,
                           blue :: a, alpha :: a }
  deriving (Eq, Show)

type CColorCoord = SfUint8
type ColorCoord = Word8

type CColor = ColorType CColorCoord
type Color = ColorType ColorCoord

instance Functor ColorType where
  fmap f (Color r g b a) = Color (f r) (f g) (f b) (f a)

instance Storable CColor where
  sizeOf _ = (#size sfColor)
  alignment _ = (#alignment sfColor)
  peek ptr = do
    r <- (#peek sfColor, r) ptr
    g <- (#peek sfColor, g) ptr
    b <- (#peek sfColor, b) ptr
    a <- (#peek sfColor, a) ptr
    return $! Color r g b a
  poke ptr (Color r g b a) = do
    (#poke sfColor, r) ptr r
    (#poke sfColor, g) ptr g
    (#poke sfColor, b) ptr b
    (#poke sfColor, a) ptr a

instance Convertible_HS_C Color (Ptr CColor) IO where
  withHs color f = with (fmap fromIntegral color) f
  createFromC cPtr = fmap fromIntegral `fmap` peek cPtr

