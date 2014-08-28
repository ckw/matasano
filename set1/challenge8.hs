import           Crypto.Common ( asciiToHex'
                               , hammingDistances
                               , hexToAscii'
                               )
import qualified Data.ByteString.Lazy as BL
import           Data.List (sortBy)
import           Data.Function (on)
import           Control.Applicative ((<$>))
import           Prelude hiding (lines)

{-
takes a list of lines on stdin, sorts them by the minimum hamming distance
between any pair of 16 byte blocks within each line.
example:

< 8.txt |../dist/build/set1ch8/set1ch8
 -}

main :: IO ()
main = do lines <-  BL.split 0xa . BL.init <$> BL.getContents
          let lines' = fmap (asciiToHex' . BL.unpack) lines
          mapM_ print $ sortBy (flip compare `on` snd) . fmap collect $ lines'
  where collect line = (BL.pack $ hexToAscii' line, minimum $ hammingDistances line)
