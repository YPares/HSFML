module SFML.Direct.System.Randomizer where

import SFML.Direct.CImport.Randomizer


setSeed :: Int -> IO ()
setSeed seed = c_Random_SetSeed $ fromIntegral seed

getSeed :: IO Int
getSeed = fmap fromIntegral $ c_Random_GetSeed

randomFloat :: Float -> Float -> IO Float
randomFloat begin end =
  fmap realToFrac $ c_Random_Float (realToFrac begin) (realToFrac end)

randomInt :: Int -> Int -> IO Int
randomInt begin end =
  fmap fromIntegral $ c_Random_Int (fromIntegral begin) (fromIntegral end)

