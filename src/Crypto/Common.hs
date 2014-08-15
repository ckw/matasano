module Crypto.Common
( hexToAscii
, lbsXOR
, asciiToHex
, toB64
, genCandidates
)
where

import           Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack, unpack)
import           Data.Char (ord, chr)
import qualified Data.Map.Strict as M
import           Data.Word

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


genCandidates :: BL.ByteString -> [((Char, BL.ByteString), Double)]
genCandidates lbs = fmap distToEnglish . charStringPairs $ asciiToHex lbs
  where charStringPairs s = fmap (xorAndConvertToString s) singleCharStrings
        xorAndConvertToString lbs2 (c, lbs1) = (c, BL8.unpack $ lbsXOR lbs1 lbs2)
        distToEnglish (c, l) = ((c, BL8.pack l), totalDistance l)
        singleCharStrings = let f c = (chr c, BL8.pack . repeat . chr $ c)
                            in fmap f [0..127]

--TODO this algorithm shouldn't work as well as it does. Find another one.
totalDistance :: [Char] -> Double
totalDistance str = let len = fromIntegral $ length str
                        counts = countChars str
                        percentages = M.map (/ len) counts
                    in sum . M.elems $ M.mapWithKey distFromCanonical percentages
  where distFromCanonical k v = case M.lookup k frequenciesCanonical of
                                    Nothing -> v
                                    Just d -> abs (v - d)

countChars :: [Char] -> M.Map Char Double
countChars str = let charCounts = foldr incr M.empty (fmap toLower str)
                 in  M.union charCounts frequenciesCanonical
  where incr c m = let c' = collapseNonAlpha c
                   in case M.lookup c m of
                        Nothing -> M.insert c' 1 m
                        Just i -> M.insert c' (i + 1) m


frequenciesCanonical :: M.Map Char Double
frequenciesCanonical = M.fromList [ ('a', 0.08167)
                                  , ('b', 0.01492)
                                  , ('c', 0.02782)
                                  , ('d', 0.04253)
                                  , ('e', 0.13000)
                                  , ('f', 0.02228)
                                  , ('g', 0.02015)
                                  , ('h', 0.06094)
                                  , ('i', 0.06966)
                                  , ('j', 0.00153)
                                  , ('k', 0.00772)
                                  , ('l', 0.04025)
                                  , ('m', 0.02406)
                                  , ('n', 0.06749)
                                  , ('o', 0.07507)
                                  , ('p', 0.01929)
                                  , ('q', 0.00095)
                                  , ('r', 0.05987)
                                  , ('s', 0.06327)
                                  , ('t', 0.09056)
                                  , ('u', 0.02758)
                                  , ('v', 0.00978)
                                  , ('w', 0.02360)
                                  , ('x', 0.00150)
                                  , ('y', 0.01974)
                                  , ('z', 0.00074)
                                  , (' ', 0.14000)
                                  -- catchall for digits, punctuation, etc.
                                  , ('P', 0.07000)
                                  ]

toLower :: Char -> Char
toLower c = if ord c < 0x5b && ord c > 0x40 then chr (ord c + 0x20) else c

-- convert printable, non-lowercase alphabetic characters to 'P'
collapseNonAlpha :: Char -> Char
collapseNonAlpha c = if ord c < 0x7b && ord c > 0x60 || ord c < 0x21 then c else 'P'
