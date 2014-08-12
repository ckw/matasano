module Crypto.Common
( toHex
, toB64
)
where

import           Data.Bits ((.|.), shiftL)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Data.Char (chr)

toB64 :: BL.ByteString -> BL.ByteString
toB64 = B64.encode

toHex :: BL.ByteString -> BL.ByteString
toHex bs = BL.pack $ toHex' (BL.unpack bs)
  where toHex' :: [Word8] -> [Word8]
        toHex' [] = []
        toHex' [x] = [shiftL x 4]
        toHex' (x:y:xs) = ((shiftL (conv x) 4) .|. (conv y)) : toHex' xs
        conv :: Word8 -> Word8
        conv x = case x of
            0x30 -> 0x0
            0x31 -> 0x1
            0x32 -> 0x2
            0x33 -> 0x3
            0x34 -> 0x4
            0x35 -> 0x5
            0x36 -> 0x6
            0x37 -> 0x7
            0x38 -> 0x8
            0x39 -> 0x9

            0x41 -> 0xa
            0x42 -> 0xb
            0x43 -> 0xc
            0x44 -> 0xd
            0x45 -> 0xe
            0x46 -> 0xf

            0x61 -> 0xa
            0x62 -> 0xb
            0x63 -> 0xc
            0x64 -> 0xd
            0x65 -> 0xe
            0x66 -> 0xf

            c -> error $ "invalid hex character: " ++ (show . chr . fromIntegral $ c)
