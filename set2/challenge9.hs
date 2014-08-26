import           Crypto.Common (padBlock)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack, unpack)

main :: IO ()
main = do let initBlock = BL8.pack "YELLOW SUBMARINE"
          putStrLn $ "initial block: " ++ (BL8.unpack initBlock)
          putStrLn $ "after padding: "
                  ++ (show $ BL.pack $ padBlock 20 (BL.unpack initBlock))
