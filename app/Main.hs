module Main
  ( main
  ) where

import           Fingerprint
import           Query
import           System.Directory
import qualified System.IO.Strict              as S


dir :: [Char]
dir = "../../abstract/plagiarism-dataset/src/A2016/Z1/Z1/"

main :: IO ()
main = do
  fingerprints <- listDirectory dir
    >>= mapM (\path -> fingerprint path w k <$> S.readFile (dir ++ path))
  print $ allToAll t fingerprints
 where
  w = 100
  k = 50
  t = 3
