{-#LANGUAGE DeriveDataTypeable #-}

import           Crypto.Common ( asciiToHex
                               , fromB64
                               , encryptCBC
                               , hammingDistances
                               , padBlock
                               )
import qualified Crypto.Cipher.AES as AES
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import           Data.List (tails)
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad (join, when)
import           Data.Char (toLower)
import           Data.Typeable
import           Data.Word
import           System.Console.GetOpt
import           System.Exit (exitSuccess)
import           System.Environment (getArgs)
import           System.Random

{-
 - detects AES ECB mode
Usage: ch11 [OPTIONS]
  -e [base64, hex]  --encoding=[base64, hex]  input string encoding
  -h                --help                    show help

example:

echo -n "yellow submarineyellow submarineyellow submarine" | ./dist/build/set2ch11/set2ch11
-}

main :: IO ()
main = do (opts, _) <- join $ parseOpts <$> getArgs
          when (help opts) $
              putStrLn (usageInfo "Usage: ch11 [OPTIONS]" options) >> exitSuccess
          stdin <- BL.getContents
          gen <- newStdGen
          let remNewLines = BL.filter (/= 0xa)
              decodeF = case encoding opts of
                            Base64 -> fromB64 . remNewLines
                            Hex -> asciiToHex . remNewLines
                            Raw -> id
              decoded = BL.unpack . decodeF $ stdin
              (aesMode, ciphertext) = encryptionOracle gen decoded
              aesModeGuess = detectAESMode (BS.unpack ciphertext)
          putStrLn $ "encrypted stdin using AES "
                  ++ (show aesMode)
                  ++ " mode; detected as "
                  ++ (show aesModeGuess)
                  ++ " mode"

randBytes :: StdGen -> Int -> [Word8]
randBytes gen num = take num $ randomRs (0, 255) gen

randAESKey :: StdGen -> [Word8]
randAESKey gen = randBytes gen 16

rand5To10 :: StdGen -> Int
rand5To10 gen = fst $ randomR (5, 10) gen

randBool :: StdGen -> Bool
randBool = fst . random

encryptionOracle :: StdGen -> [Word8] -> (AESMode, BS.ByteString)
encryptionOracle gen0 plaintext =
    let (gen1, gen2) = split gen0
        (gen3, gen4) = split gen1
        (gen5, gen6) = split gen2
        useECB = randBool gen0
        prepad = randBytes gen1 (rand5To10 gen2)
        postpad = randBytes gen3 (rand5To10 gen4)
        iv = randAESKey gen5
        key = randAESKey gen6
        aes = AES.initAES $ BS.pack key
        plaintext' = prepad ++ plaintext ++ postpad
    in if useECB
       then let pt'' = BS.pack $ padBlock 16 plaintext'
            in (ECB, AES.encryptECB aes pt'')
       else (CBC, BS.pack $ encryptCBC iv key plaintext')

data AESMode = ECB | CBC
  deriving Show

maxPad :: Int
maxPad = 10

detectAESMode :: [Word8] -> AESMode
detectAESMode ws = let hds = concat . fmap hammingDistances . take maxPad $ tails ws
                   in if minimum hds == 0 then ECB else CBC

options :: [OptDescr (Options -> Options)]
options = [ Option "e" ["encoding"]     (ReqArg setEnc "[base64, hex]")
                "input string encoding"
          , Option "h" ["help"]         (NoArg setSH) "show help"
          ]
  where setSH    o = o {help = True}
        setEnc s o = o {encoding = pEnc s}
        pEnc s = let s' = toLower <$> s
                     errStr = '(': s
                           ++ ") is not a valid encoding; use base64 or hex"
                 in case s' of
                     "base64" -> Base64
                     "hex" -> Hex
                     _ -> throw $ InvalidArg errStr


data OptException = InvalidArg String
    deriving (Show, Typeable)

instance Exception OptException

data Options = Options
    { encoding :: Encoding
    , decrypt  :: Bool
    , help     :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { encoding  = Raw
    , decrypt   = False
    , help      = False
    }

data Encoding = Base64
              | Hex
              | Raw

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv = case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldr ($) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: "
