{-#LANGUAGE DeriveDataTypeable #-}

import           Crypto.Common ( asciiToHex
                               , decryptCBC
                               , encryptCBC
                               , fromB64
                               , toB64S
                               )
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
encrypt/decrypt input using AES in CBC mode. Expects cipher text on stdin
and key as arg. Encrypts by default; initialization vector is zeroed

Usage: ch10 [OPTIONS] <key>
  -e [base64, hex]  --encoding=[base64, hex]  input string encoding
  -d                --decrypt                 decrypt the input
  -h                --help                    show help

example:

< 10.txt |../dist/build/set2ch10/set2ch10 -d -e base64 "YELLOW SUBMARINE"
 -}

main :: IO ()
main = do (opts, nonOpts) <- join $ parseOpts <$> getArgs
          when (help opts || null nonOpts) $
              putStrLn (usageInfo "Usage: ch10 [OPTIONS] <key>" options) >> exitSuccess
          stdin <- BL.getContents
          let remNewLines = BL.filter (/= 0xa)
              decodeF = case encoding opts of
                            Base64 -> fromB64 . remNewLines
                            Hex -> asciiToHex . remNewLines
                            Raw -> id
              decoded = BS.unpack . toStrict . decodeF $ stdin
              key = BS.unpack . BS8.pack $ head nonOpts
              defaultIV = replicate (length key) 0
          if decrypt opts
          then BS8.putStrLn . BS.pack $ decryptCBC defaultIV key decoded
          else BS8.putStrLn . toB64S . BS.pack $ encryptCBC defaultIV key decoded
  where toStrict = BS.concat . BL.toChunks


options :: [OptDescr (Options -> Options)]
options = [ Option "e" ["encoding"]     (ReqArg setEnc "[base64, hex]")
                "input string encoding"
          , Option "d" ["decrypt"]      (NoArg setDec)
                "decrypt the input"
          , Option "h" ["help"]         (NoArg setSH) "show help"
          ]
  where setSH    o = o {help = True}
        setDec   o = o {decrypt = True}
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
