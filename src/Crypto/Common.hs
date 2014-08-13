module Crypto.Common
( hexToAscii
, lbsXOR
, asciiToHex
, toB64
)
where

import           Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Data.Char (chr)

toB64 :: BL.ByteString -> BL.ByteString
toB64 = B64.encode

asciiToHex :: BL.ByteString -> BL.ByteString
asciiToHex = BL.pack . asciiToHex' . BL.unpack
  where asciiToHex' :: [Word8] -> [Word8]
        asciiToHex' [] = []
        asciiToHex' [x] = [shiftL x 4]
        asciiToHex' (x:y:xs) = ((shiftL (conv x) 4) .|. (conv y)) : asciiToHex' xs
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

hexToAscii :: BL.ByteString -> BL.ByteString
hexToAscii = BL.pack . hexToAscii' . BL.unpack
  where hexToAscii' :: [Word8] -> [Word8]
        hexToAscii' [] = []
        hexToAscii' (x:xs) = let h1 = shiftR (x .&. 0xf0) 4
                                 h2 = x .&. 0x0f
                             in conv h1 : conv h2 : hexToAscii' xs
        conv :: Word8 -> Word8
        conv x = case x of
           0x0 -> 0x30
           0x1 -> 0x31
           0x2 -> 0x32
           0x3 -> 0x33
           0x4 -> 0x34
           0x5 -> 0x35
           0x6 -> 0x36
           0x7 -> 0x37
           0x8 -> 0x38
           0x9 -> 0x39

           0xa -> 0x61
           0xb -> 0x62
           0xc -> 0x63
           0xd -> 0x64
           0xe -> 0x65
           0xf -> 0x66

           _ -> error "impossible1"

lbsXOR :: BL.ByteString -> BL.ByteString -> BL.ByteString
lbsXOR bs1 bs2 = let ws1 = BL.unpack bs1
                     ws2 = BL.unpack bs2
                 in BL.pack $ zipWith xor ws1 ws2
