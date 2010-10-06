{-# LANGUAGE MultiParamTypeClasses #-}

module SFML.Direct.System.Clock where

import SFML.Direct.CImport.Clock
import SFML.Direct.Types.Clock
import SFML.Direct.Classes.Convertible_HS_C


createClock :: IO Clock
createClock = c_Clock_Create >>= createFromC

getTime :: Clock -> IO Float
getTime clk = withHs clk $ \clkPtr ->
  fmap realToFrac $ c_Clock_GetTime clkPtr

reset :: Clock -> IO ()
reset clk = withHs clk $ \clkPtr ->
  c_Clock_Reset clkPtr

