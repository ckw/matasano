import qualified Data.ByteString.Lazy as BL
import           Crypto.Common (toB64, asciiToHex)

main :: IO ()
main = BL.getContents >>= BL.putStr . toB64 . asciiToHex
