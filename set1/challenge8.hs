import           Crypto.Common ( asciiToHex'
                               , hammingDistance'
                               , hexToAscii'
                               )
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as D
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Word
import           Control.Applicative ((<$>))
import           Control.Monad (guard)
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
  where collect line =  (BL.pack $ hexToAscii' line, minimum $ hammingDistances line)


hammingDistances :: [Word8] -> [Int]
hammingDistances wss = do let blocks = zip [(0 :: Integer)..] (piecesOfN 16 wss)
                          a <- blocks
                          b <- blocks
                          guard (fst a < fst b)
                          return $ hammingDistance' (snd a) (snd b)


piecesOfN :: Int -> [a] -> [[a]]
piecesOfN n xs = D.toList $ piecesOfN' n xs D.empty

piecesOfN' :: Int -> [a] -> D.DList [a] -> D.DList [a]
piecesOfN' n xs acc = let (start, rest) = splitAt n xs
                      in if null rest
                         then D.snoc acc start
                         else piecesOfN' n rest (D.snoc acc start)
