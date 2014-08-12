import qualified Data.ByteString.Lazy as BL
import           Crypto.Common (toB64, toHex)

main :: IO ()
main = BL.getContents >>= BL.putStr . toB64 . toHex
