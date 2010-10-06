{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Control.Concurrent

import qualified SFML.Direct.Graphics as Sf
import qualified SFML.Utils as Sf
import qualified Data.ByteString as BS


getSprite = do
  (Just img) <- Sf.createImageFromFile "ankh-morpork.png"
  spr <- Sf.createSpriteFromImage img
  Sf.getSize spr >>= Sf.setCenter spr . Sf.bis (/2)
  return spr

main = do
  thId <- forkIO $ sequence_ $ repeat $ getLine >>= (putStrLn . (">> "++))

  (Just win) <- Sf.createWindow (800, 600, 32) "Window" [Sf.Close] (24, 8, 0)
  spr <- getSprite
  txt <- Sf.createText
  --Sf.createFontFromFile "DejaVuSans.ttf" 30 (Just $ ['\1'..'\xff'] ++ "ᐰ") >>= 
  --  Sf.setFont txt
  Sf.setString txt "aaa uuu ééé ççç ᐰ ëëë _"
  Sf.setStyle txt [Sf.TextUnderlined, Sf.TextItalic]

  Sf.setFramerateLimit win 100
  Sf.useVerticalSync win True

  Sf.loopIfM_ (Sf.isOpened win) $ do

    Sf.maybeLoopM_ (Sf.getEvent win) $ \evt -> case evt of
      Sf.EvtClosed -> Sf.close win
      Sf.EvtKeyPressed {Sf.code=Sf.KeyEscape} -> Sf.close win
      _ -> return ()

    Sf.clear win $ Sf.fromRGB 0 0 0
    
    Sf.getInput win >>= Sf.getMousePosition >>=
      Sf.setPosition spr . Sf.convert

    win `Sf.draw` spr
    win `Sf.draw` txt

    --ft <- Sf.getFrameTime win
    --when (ft > 0) $ putStrLn $ "FPS: "++show (1/ft)
    
    Sf.display win

  killThread thId

