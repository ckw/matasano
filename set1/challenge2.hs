import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Crypto.Common (hexToAscii, asciiToHex, lbsXOR)
import           System.Environment (getArgs)

--xor two hex strings, arg1 and arg2
main :: IO ()
main = do [lbs1, lbs2] <- fmap (fmap BL8.pack . take 2) $ getArgs
          BL.putStr . hexToAscii $ lbsXOR (asciiToHex lbs1) (asciiToHex lbs2)
