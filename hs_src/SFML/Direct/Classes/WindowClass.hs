{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module SFML.Direct.Classes.WindowClass where

import SFML.Direct.Types.VideoMode
import SFML.Direct.Types.Event
import SFML.Direct.Types.Input
import SFML.Direct.Types.Enums
import SFML.Direct.Classes.Handling


class (Resizable w Int m, Positionable w Int m) => 
      WindowClass w m where
  createWindow :: (Int, Int, Int) -> String -> [WindowStyle] -> (Int, Int, Int) -> m (Maybe w)
  --createFromHandle
  close :: w -> m ()
  isOpened :: w -> m Bool
  getSettings :: w -> m (Int, Int, Int)
  getEvent :: w -> m (Maybe Event)
  useVerticalSync :: w -> Bool -> m ()
  showMouseCursor :: w -> Bool -> m ()
  setCursorPosition :: w -> (Int, Int) -> m ()
  showWindow :: w -> Bool -> m ()
  enableKeyRepeat :: w -> Bool -> m ()
  --setIcon
  setActive :: w -> Bool -> m ()
  display :: w -> m ()
  getInput :: w -> m Input
  setFramerateLimit :: w -> Int -> m ()
  getFrameTime :: w -> m Float
  setJoystickThreshold :: w -> Float -> m ()

