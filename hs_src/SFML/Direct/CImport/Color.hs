{-# LANGUAGE ForeignFunctionInterface #-}

module SFML.Direct.CImport.Color where

import Foreign

import SFML.Direct.Types.Color


foreign import ccall unsafe "wrp_sfColor_FromRGB"
  c_Color_FromRGB :: CColorCoord -> CColorCoord -> CColorCoord ->
                     Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfColor_FromRGBA"
  c_Color_FromRGBA :: CColorCoord -> CColorCoord -> CColorCoord -> CColorCoord ->
                      Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfColor_Add"
  c_Color_Add :: Ptr CColor -> Ptr CColor ->
                 Ptr CColor -> IO ()

foreign import ccall unsafe "wrp_sfColor_Modulate"
  c_Color_Modulate :: Ptr CColor -> Ptr CColor -> 
                      Ptr CColor -> IO ()

