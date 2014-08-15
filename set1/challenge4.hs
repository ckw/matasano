import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (putStrLn)
import           Data.List (sortBy, intersect)
import           Data.Function (on)
import           Crypto.Common (genCandidates)
import           System.Environment (getArgs)
import           Text.Printf (printf)

{-
takes the first non-flag argument as the path to a file f
generates 1 byte xor candidates for each line in f
prints the most promising candidate, unless -a or --display-all flag is included,
in which case it prints all candidates in ascending order of promise

example:
./dist/build/set1ch4/set1ch4 'set1/4.txt'
-}

main :: IO ()
main = do displayAll <- fmap (not . null . intersect ["-a", "--display-all"]) $ getArgs
          fileN <- fmap (head . filter (not . flip elem ["-a", "--display-all"])) $ getArgs
          lbss <- fmap (BL.split 0xa) $ BL.readFile fileN
          let results = sortBy (flip compare `on` snd) $ concatMap genCandidates lbss

          if displayAll
          then mapM_ formatAndPrint results
          else BL8.putStrLn . snd . fst . last $ results
  where formatAndPrint ((c, str), dist) = printf fStr (show c) dist (show str)
        fStr = "%-6s : %.4f : %s\n"
