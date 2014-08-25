import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack, unpack)
import           Data.Word

main :: IO ()
main = do let initBlock = BL8.pack "YELLOW SUBMARINE"
          putStrLn $ "initial block: " ++ (BL8.unpack initBlock)
          putStrLn $ "after padding: "
                  ++ (show $ BL.pack $ padBlock 20 (BL.unpack initBlock))

padBlock :: Int -> [Word8] -> [Word8]
padBlock sz ws = let diff = sz - length ws
                 in ws ++ replicate (fromIntegral diff) (fromIntegral diff)
