module Main where

import Fingerprint


main :: IO ()
main = do
  contents <- readFile "../../abstract/plagiarism-dataset/src/A2016/Z1/Z1/student1013.c"
  print $ fingerprint 4 10 contents
