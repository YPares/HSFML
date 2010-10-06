{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module SFML.Utils where

import Control.Monad
--import Data.Encoding (encodeStrictByteString, decodeStrictByteString)
--import Data.Encoding.UTF32
--import Data.Encoding.UTF8
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8


loopIfM_ mcond action = do
  b <- mcond
  when b $ do action
              loopIfM_ mcond action

maybeLoopM_ maybeAction whatToDo = do
  res <- maybeAction
  case res of
    Just x -> whatToDo x >> maybeLoopM_ maybeAction whatToDo
    Nothing -> return ()

bis :: (a -> b) -> (a, a) -> (b, b)
bis f (x, y) = (f x, f y)

class Convertible a b where
  convert :: a -> b

instance Convertible (Int, Int) (Float, Float) where
  convert = bis fromIntegral

instance Convertible (Float, Float) (Int, Int) where
  convert = bis floor


--toUTF32ByteString :: String -> BS.ByteString
--toUTF32ByteString = encodeStrictByteString UTF32

--fromUTF32ByteString :: BS.ByteString -> String
--fromUTF32ByteString = decodeStrictByteString UTF32

--toUTF8ByteString :: String -> BS.ByteString
--toUTF8ByteString = encodeStrictByteString UTF8

--fromUTF8ByteString :: BS.ByteString -> String
--fromUTF8ByteString = decodeStrictByteString UTF8

toUTF8ByteString :: String -> BS.ByteString
toUTF8ByteString = U8.fromString

fromUTF8ByteString :: BS.ByteString -> String
fromUTF8ByteString = U8.toString

