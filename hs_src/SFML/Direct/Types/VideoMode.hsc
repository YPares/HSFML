{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances, MultiParamTypeClasses #-}

module SFML.Direct.Types.VideoMode where

import Foreign
import Foreign.C.Types

import SFML.Direct.Classes.Convertible_HS_C

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

#include <Wrappers.h>


instance Storable (CUInt, CUInt, CUInt) where
  sizeOf _ = (#size sfVideoMode)
  alignment _ = (#alignment sfVideoMode)
  peek ptr = do
    a <- (#peek sfVideoMode, Width) ptr
    b <- (#peek sfVideoMode, Height) ptr
    c <- (#peek sfVideoMode, BitsPerPixel) ptr
    return $! (a,b,c)
  poke ptr (a,b,c) = do
    (#poke sfVideoMode, Width) ptr a
    (#poke sfVideoMode, Height) ptr b
    (#poke sfVideoMode, BitsPerPixel) ptr c
-- We work with sfVideoMode, but it works with every other struct which
-- contains 3 unsigned ints

convInt3Tuple (a,b,c) = (fromIntegral a, fromIntegral b, fromIntegral c)

instance Convertible_HS_C (Int, Int, Int) (Ptr (CUInt, CUInt, CUInt)) IO where
  withHs x f = with (convInt3Tuple x) f
  createFromC ptr = convInt3Tuple `fmap` peek ptr

