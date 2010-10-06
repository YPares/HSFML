module SFML.Direct.Window.VideoMode where

import Foreign
import Data.List (unfoldr)
import Control.Monad (guard)

import SFML.Direct.CImport.VideoMode
import SFML.Direct.Types.VideoMode
import SFML.Direct.Classes.Convertible_HS_C


getDesktopMode :: IO (Int, Int, Int)
getDesktopMode = do
  withHs (0::Int, 0::Int, 0::Int) $ \vmPtr -> do
    c_VideoMode_GetDesktopMode vmPtr
    createFromC vmPtr

getModes :: IO [(Int, Int, Int)]
getModes = do
  count <- c_VideoMode_GetModesCount
  sequence $ unfoldr (\i -> guard (i<count) >> Just (go i, i+1)) 0
  where go index = do
          withHs (0::Int, 0::Int, 0::Int) $ \vmPtr -> do
            c_VideoMode_GetMode (fromIntegral index) vmPtr
            createFromC vmPtr

isValid :: (Int, Int, Int) -> IO Bool
isValid mode = withHs mode $ \ptr ->
  c_VideoMode_IsValid ptr

