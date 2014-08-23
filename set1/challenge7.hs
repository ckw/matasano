{-#LANGUAGE DeriveDataTypeable #-}

import           Crypto.Common ( asciiToHex
                               , fromB64
                               )
import           Crypto.Cipher.AES
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad (join, when)
import           Data.Char (toLower)
import           Data.Typeable
import           System.Console.GetOpt
import           System.Exit (exitSuccess)
import           System.Environment (getArgs)

{-
Decrypts AES in ECB mode. Expects cipher text on stdin and key as arg

Usage: ch7 [OPTIONS] <key>
-e [base64, hex]  --encoding=[base64, hex]  input string encoding
-h                --help                    show help

example:

< 7.txt |../dist/build/set1ch7/set1ch7  -e base64
 -}

main :: IO ()
main = do (opts, nonOpts) <- join $ parseOpts <$> getArgs
          when (help opts || null nonOpts) $
              putStrLn (usageInfo "Usage: ch7 [OPTIONS] <key>" options) >> exitSuccess
          stdin <- BL.getContents
          let remNewLines = BL.filter (/= 0x0a)
              decodeF = case encoding opts of
                            Base64 -> fromB64 . remNewLines
                            Hex -> asciiToHex . remNewLines
                            Raw -> id
              decoded = toStrict . decodeF $ stdin
              aes = initAES (BS8.pack $ head nonOpts)
          BS8.putStrLn $ decryptECB aes decoded
  where toStrict = BS.concat . BL.toChunks



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
    , help     :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { encoding  = Raw
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
