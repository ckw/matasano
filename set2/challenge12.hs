{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveDataTypeable #-}

import           Crypto.Common ( fromB64
                               , padBlock
                               , randAESKey
                               )
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import           Control.Applicative ((<$>))
import           Data.Char (chr)
import           Data.Word
import           System.Random (newStdGen)

--ugh
--to run: ./dist/build/set2ch12/set2ch12

main :: IO ()
main = do key <- newStdGen >>= return . BS.pack . randAESKey
          putStrLn $ (chr . fromIntegral) <$> breakECB 16 key

breakECB :: Int -> BS.ByteString -> [Word8]
breakECB blockSize key = breakECB' (blockSize - 1) [] 1
  where aes = AES.initAES key
        oracle ws = AES.encryptECB aes . BS.pack . padBlock 16 $ ws ++ secretText
        secretText = BL.unpack . fromB64 . BL.concat $ [ "Um9sbGluJyBpbiBteSA1LjAK"
                                                       , "V2l0aCBteSByYWctdG9wIGRv"
                                                       , "d24gc28gbXkgaGFpciBjYW4g"
                                                       , "YmxvdwpUaGUgZ2lybGllcyBv"
                                                       , "biBzdGFuZGJ5IHdhdmluZyBq"
                                                       , "dXN0IHRvIHNheSBoaQpEaWQg"
                                                       , "eW91IHN0b3A/IE5vLCBJIGp1"
                                                       , "c3QgZHJvdmUgYnkK"
                                                       ]
        breakECB' padSz acc blk =
            let pad = replicate padSz 0
                pre = pad ++ acc
                o = oracle pad
                ebs = [ (AES.encryptECB aes . BS.pack $ pre ++ [w], w) |
                        w <- [0..255]
                      ]
                Just c = lookup (BS.take (blk * blockSize) o) ebs
            in if blk * blockSize > length secretText
               then acc
               else if padSz == 0
                    then breakECB' (blockSize - 1) (acc ++ [c]) (blk + 1)
                    else breakECB' (padSz - 1) (acc ++ [c]) blk
