module Main where

import           Fingerprint
import           Query


main :: IO ()
main = do
  doc1 <- readFile
    "../../abstract/plagiarism-dataset/src/A2016/Z1/Z1/student7386.c"
  doc2 <- readFile
    "../../abstract/plagiarism-dataset/src/A2016/Z1/Z1/student9538.c"
  print $ matches (fingerprint "student7386" w k doc1)
                  (fingerprint "student9538" w k doc2)
 where
  w = 100
  k = 50
