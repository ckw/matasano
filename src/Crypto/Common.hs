module Crypto.Common
( hexToAscii
, hexToAscii'
, lbsXOR
, asciiToHex
, toB64
, genCandidates
, w8sXOR
)
where

import           Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord, chr)
import           Control.Arrow (first, second)
import           Control.Applicative ((<$>))
import           Debug.Trace
import qualified Data.Map.Strict as M
import           Data.Word

--for debugging only
_p :: Show a => [Char] -> a -> a
_p str a = trace (str ++ show a) a

toB64 :: BL.ByteString -> BL.ByteString
toB64 = B64.encode

asciiToHex :: BL.ByteString -> BL.ByteString
asciiToHex = BL.pack . asciiToHex' . BL.unpack

asciiToHex' :: [Word8] -> [Word8]
asciiToHex' [] = []
asciiToHex' [x] = [shiftL x 4]
asciiToHex' (x:y:xs) = ((shiftL (conv x) 4) .|. (conv y)) : asciiToHex' xs
  where conv :: Word8 -> Word8
        conv w = case w of
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

            _ -> error $ "invalid hex character: " ++ (show . chr . fromIntegral $ w)

hexToAscii :: BL.ByteString -> BL.ByteString
hexToAscii = BL.pack . hexToAscii' . BL.unpack

hexToAscii' :: [Word8] -> [Word8]
hexToAscii' [] = []
hexToAscii' (x:xs) = let h1 = shiftR (x .&. 0xf0) 4
                         h2 = x .&. 0x0f
                     in conv h1 : conv h2 : hexToAscii' xs
  where conv :: Word8 -> Word8
        conv w = case w of
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
lbsXOR bs1 bs2 = BL.pack $ lbsXOR' bs1 bs2

lbsXOR' :: BL.ByteString -> BL.ByteString -> [Word8]
lbsXOR' bs1 bs2 = let ws1 = BL.unpack bs1
                      ws2 = BL.unpack bs2
                  in w8sXOR ws1 ws2


w8sXOR :: [Word8] -> [Word8] -> [Word8]
w8sXOR w8s1 w8s2 = zipWith xor w8s1 w8s2


genCandidates :: BL.ByteString -> [((Char, BL.ByteString), Double)]
genCandidates lbs =
    let hex = asciiToHex' . BL.unpack $ lbs
    in rank . label <$> [(w, w8sXOR hex $ repeat w) | w <- [0..127]]
  where rank = second totalDistance
        label (w, l) = ((chr $ fromIntegral w, BL.pack l), l)


totalDistance :: [Word8] -> Double
totalDistance str =
    let len = fromIntegral $ length str
        counts = countChars str
        percentages =(/ len) . fromIntegral <$> counts
        percentages' = M.union percentages frequenciesCanonical
    in sum . M.elems $ M.mapWithKey deviation percentages'
  where deviation k v = case M.lookup k frequenciesCanonical of
                            Nothing -> v
                            Just d -> abs (v - d)


countChars :: [Word8] -> M.Map Word8 Int
countChars str = foldr incr M.empty (toLower <$> str)
  where incr c m = let c' = collapseNonAlpha c
                       --penalize gibberish (or at least improbable) bytes
                       weight = if c' < 0x20 || c' > 0x7e then 5 else 1
                   in case M.lookup c' m of
                        Nothing -> M.insert c' weight m
                        Just i -> M.insert c' (i + weight) m


-- from http://en.wikipedia.org/wiki/Letter_frequency
frequenciesCanonical :: M.Map Word8 Double
frequenciesCanonical = M.fromList $ fmap (first (fromIntegral . ord))
    [ ('a', 0.08167)
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

toLower :: Word8 -> Word8
toLower c = if c > 0x40 && c < 0x5b then c + 0x20 else c

-- convert printable, non-lowercase alphabetic characters to 'P'
collapseNonAlpha :: Word8 -> Word8
collapseNonAlpha c = if (c > 0x20 && c < 0x41) ||
                        (c > 0x5a && c < 0x61) ||
                        (c > 0x7a && c < 0x7f)
                    then 0x50
                    else c
