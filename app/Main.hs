module Main where

import           Control.Applicative            ( ZipList(..) )
import           Data.Char                      ( isAlphaNum
                                                , toLower
                                                )
import           Data.List                      ( tails )


-- | /O(n * k)/ 'kgrams' @k@ @s@ returns the list of all possible grams of size
-- @k@ in @s@
--
-- Examples:
--
--  >>> kgrams 5 "Hello, world"
-- ["hello","ellow","llowo","lowor","oworl","world"]
kgrams :: Int -> String -> [String]
kgrams k = getZipList . traverse ZipList . take k . tails . preprocess
  where preprocess = map toLower . filter isAlphaNum


hash :: Int -> String -> Int
hash = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
