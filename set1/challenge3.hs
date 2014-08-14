import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (putStrLn)
import           Data.List (sortBy, intersect)
import           Data.Function (on)
import           Crypto.Common (genCandidates)
import           System.Environment (getArgs)
import           Text.Printf (printf)

-- XORs stdin with each ASCII character, and ranks the
-- resulting strings by their character distribution's similarity to that
-- expected of english text
-- example usage:
-- echo -n '1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736' | ../set1ch3
-- use -a or --display-all to display all results

main :: IO ()
main = do displayAll <- fmap (not . null . intersect ["-a", "--display-all"]) $ getArgs
          lbs <- BL.getContents
          let results = sortBy (flip compare `on` snd)  $ genCandidates lbs
          if displayAll
          then mapM_ formatAndPrint results
          else BL8.putStrLn . snd . fst . last $ results
  where formatAndPrint ((c, str), dist) = printf fStr (show c) dist (show str)
        fStr = "%-6s : %.4f : %s\n"
