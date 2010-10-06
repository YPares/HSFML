{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.Rect where

import Foreign
import Foreign.C.Types

import SFML.Direct.Classes.Convertible_HS_C

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include <Wrappers.h>


data Rect a = Rect { left :: a, top :: a,
                     right :: a, bottom :: a }
  deriving (Eq, Show)

instance Functor Rect where
  fmap f (Rect l t r b) = Rect (f l) (f t) (f r) (f b)

instance Storable (Rect CFloat) where
  sizeOf _ = (#size sfFloatRect)
  alignment _ = (#alignment sfFloatRect)
  peek ptr = do
    l <- (#peek sfFloatRect, Left) ptr
    t <- (#peek sfFloatRect, Top) ptr
    r <- (#peek sfFloatRect, Right) ptr
    b <- (#peek sfFloatRect, Bottom) ptr
    return $! Rect l t r b
  poke ptr (Rect l t r b) = do
    (#poke sfFloatRect, Left) ptr l
    (#poke sfFloatRect, Top) ptr t
    (#poke sfFloatRect, Right) ptr r
    (#poke sfFloatRect, Bottom) ptr b

instance Storable (Rect CInt) where
  sizeOf _ = (#size sfIntRect)
  alignment _ = (#alignment sfIntRect)
  peek ptr = do
    l <- (#peek sfIntRect, Left) ptr
    t <- (#peek sfIntRect, Top) ptr
    r <- (#peek sfIntRect, Right) ptr
    b <- (#peek sfIntRect, Bottom) ptr
    return $! Rect l t r b
  poke ptr (Rect l t r b) = do
    (#poke sfIntRect, Left) ptr l
    (#poke sfIntRect, Top) ptr t
    (#poke sfIntRect, Right) ptr r
    (#poke sfIntRect, Bottom) ptr b

instance Convertible_HS_C (Rect Float) (Ptr (Rect CFloat)) IO where
  withHs rect f = with (fmap realToFrac rect) f
  createFromC ptr = fmap realToFrac `fmap` peek ptr

instance Convertible_HS_C (Rect Int) (Ptr (Rect CInt)) IO where
  withHs rect f = with (fmap fromIntegral rect) f
  createFromC ptr = fmap fromIntegral `fmap` peek ptr

