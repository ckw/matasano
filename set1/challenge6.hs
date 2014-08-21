{-#LANGUAGE DeriveDataTypeable #-}

import           Crypto.Common ( asciiToHex
                               , bestByte
                               , fromB64
                               , rankKeySizes
                               , totalDistance
                               , w8sXOR)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (putStrLn)
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad (join, when)
import           Data.Char (toLower)
import qualified Data.DList as D
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Typeable
import           Data.Word (Word8)
import           System.Console.GetOpt
import           System.Exit (exitSuccess)
import           System.Environment (getArgs)

{-
Breaks repeating key XOR cipher. Expects cipher text on stdin

Usage: ch6 [OPTIONS]
-m n              --min-key-size=n          minimum key size
-M n              --max-key-size=n          maximum key size
-a                --show-all                show all key guesses, or in conjunction with -d, decrypt with all keys
-e [base64, hex]  --encoding=[base64, hex]  input string encoding
-d                --decrypt                 decrypt the cipher text with the most promising key
-h                --help                    show help

example:

< 6.txt |../dist/build/set1ch6/set1ch6  -e base64 -d
 -}

main :: IO ()
main = do (opts, _) <- join $ parseOpts <$> getArgs
          when (help opts) $ putStrLn (usageInfo "Usage: " options) >> exitSuccess
          stdin <- BL.filter (/= 0x0a) <$> BL.getContents
          let decodeF = case encoding opts of
                            Base64 -> fromB64
                            Hex -> asciiToHex
                            Raw -> id
              decoded = BL.unpack . decodeF $ stdin
              guesses = guessKey decoded (minKey opts) (maxKey opts)
              bestGuess = head . sortBy (comparing totalDistance) $ guesses
          if showAll opts
          then if decrypt opts
               then mapM_ (print . dec decoded) $ guesses
               else mapM_ (print . BL.pack) guesses
          else if decrypt opts
               then BL8.putStrLn . dec decoded $ bestGuess
               else print . BL.pack $ bestGuess
  where dec w8s = BL.pack . w8sXOR w8s . concat . repeat


guessKey :: [Word8] -> Int -> Int -> [[Word8]]
guessKey w8s from to = let sizes = fst <$> rankKeySizes w8s from to
                           splits = flip splitByN w8s <$> sizes
                       in (fmap bestByte) <$> splits


splitByN :: Int -> [a] -> [[a]]
splitByN n ws = D.toList <$> splitByN' n ws (replicate n D.empty)

splitByN' :: Int -> [b] -> [D.DList b] -> [D.DList b]
splitByN' n ws acc = let chunk = take n ws
                         l = length chunk
                     in if l < n
                        then let (toZip,rest) = splitAt l acc
                             in comb toZip chunk ++ rest
                        else splitByN' n (drop n ws) (comb acc chunk)
  where comb = zipWith D.snoc


options :: [OptDescr (Options -> Options)]
options = [ Option "m" ["min-key-size"] (ReqArg setMin "n") "minimum key size"
          , Option "M" ["max-key-size"] (ReqArg setMax "n") "maximum key size"
          , Option "a" ["show-all"]     (NoArg setSA)
                "show all key guesses, or in conjunction with -d, decrypt with all keys"
          , Option "e" ["encoding"]     (ReqArg setEnc "[base64, hex]")
                "input string encoding"
          , Option "d" ["decrypt"]      (NoArg setDec)
                "decrypt the cipher text with the most promising key"
          , Option "h" ["help"]         (NoArg setSH) "show help"
          ]
  where setMin i o = o {minKey = read i}
        setMax i o = o {maxKey = read i}
        setSA    o = o {showAll = True}
        setSH    o = o {help = True}
        setDec   o = o {decrypt = True}
        setEnc s o = o {encoding = pEnc s}
        pEnc s = let s' = toLower <$> s
                     errStr = '(': s ++ ") is not a valid encoding; use base64 or hex"
                 in case s' of
                     "base64" -> Base64
                     "hex" -> Hex
                     _ -> throw $ InvalidArg errStr

data OptException = InvalidArg String
    deriving (Show, Typeable)

instance Exception OptException

data Options = Options
    { minKey   :: Int
    , maxKey   :: Int
    , encoding :: Encoding
    , decrypt  :: Bool
    , showAll  :: Bool
    , help     :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { minKey    =  2
    , maxKey    = 40
    , encoding  = Raw
    , decrypt   = False
    , showAll   = False
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
