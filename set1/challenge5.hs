import           Crypto.Common (asciiToHex, hexToAscii', w8sXOR)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           System.Environment (getArgs)

{-
xors stdin with the key, arg1
if -d or --decrypt flag is present,
treats the input as hex encoded in ascii
 -}

main :: IO ()
main = do args <- getArgs
          stdin <- BL.getContents
          let al = length args
          if not $ al < 3
          then putStrLn "usage: challenge4 [-d, --decrypt] <key>"
          else do let decrypt = al == 2 && elem (head args) ["-d", "--decrypt"]
                      key = fmap (fromIntegral . ord) . last $ args
                  if decrypt
                  then xorWith key id (asciiToHex stdin)
                  else xorWith key hexToAscii' stdin
  where xorWith k f =  BL.putStr
                     . BL.pack
                     . f
                     . w8sXOR (concat . repeat $ k)
                     . BL.unpack
