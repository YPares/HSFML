module SFML.Direct.System.Sleep where

import SFML.Direct.CImport.Sleep


sleep :: Float -> IO ()
sleep duration = c_Sleep $ realToFrac duration

