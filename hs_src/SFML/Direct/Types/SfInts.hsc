module SFML.Direct.Types.SfInts where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits

#include <Wrappers.h>


#if UCHAR_MAX == 0xFF
type SfInt8 = CChar
type SfUint8 = CUChar
#else
#error No 8 bit integer type for this platform
#endif

#if USHRT_MAX == 0xFFFF
type SfInt16 = CShort;
type SfUint16 = CUShort;
#elif UINT_MAX == 0xFFFF
type SfInt16 = CInt;
type SfUint16 = CUInt;
#elif ULONG_MAX == 0xFFFF
type SfInt16 = CLong;
type SfUint16 = CULong;
#else
#error No 16 bits integer type for this platform
#endif

#if USHRT_MAX == 0xFFFFFFFF
type SfInt32 = CShort;
type SfUint32 = CUShort;
#elif UINT_MAX == 0xFFFFFFFF
type SfInt32 = CInt;
type SfUint32 = CUInt;
#elif ULONG_MAX == 0xFFFFFFFF
type SfInt32 = CLong;
type SfUint32 = CULong;
#else
#error No 32 bits integer type for this platform
#endif

byteStringToSfUint32List :: BS.ByteString -> [SfUint32]
byteStringToSfUint32List = go []
  where go listAcc bs 
          | bs == BS.empty = reverse listAcc
          | otherwise      = let (fourW8, bsRest) = BS.splitAt 4 bs in
              go (bsToUint32 fourW8 : listAcc) bsRest
        bsToUint32 = fst . BS.foldr' doOr (0, 0)
          where doOr w (uint32, s) =
                  (uint32 .|. (fromIntegral w `shiftL` (s*8)), s+1)

sfUint32ListToByteString :: [SfUint32] -> BS.ByteString
sfUint32ListToByteString = foldr consUint32ToBS BS.empty
  where
    consUint32ToBS uint32 = doCons 0
      where doCons s bsAcc 
              | s == 4    = bsAcc
              | otherwise = doCons (s+1) $ 
                  (fromIntegral $ uint32 `shiftR` (s*8)) `BS.cons` bsAcc

