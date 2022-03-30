module Main
  ( main
  ) where

import           Fingerprint                    ( fingerprint )
import           Query                          ( allToAll )
import           System.Directory               ( listDirectory )
import           System.Environment             ( getArgs )
import qualified System.IO.Strict              as S


main :: IO ()
main = do
  dir          <- head <$> getArgs
  fingerprints <- listDirectory dir
    >>= mapM (\path -> fingerprint path w k <$> S.readFile (dir ++ path))
  print $ allToAll t fingerprints
 where
  w = 100
  k = 50
  t = 3
