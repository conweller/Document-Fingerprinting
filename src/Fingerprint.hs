module Fingerprint
  ( fingerprint
  ) where

import           Control.Monad.Trans.State      ( evalState
                                                , state
                                                )
import           Data.Char                      ( isAlphaNum
                                                , toLower
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.Tuple.Extra               ( dupe )
import qualified Token
import           Token                          ( Token )

getTokens :: String -> [Token]
getTokens = map (Token.fromChar . toLower) . filter isAlphaNum

hashes :: Int -> String -> [Int]
hashes k s = (evalState . traverse state)
  (zipWith hashWithPrev (0 : tokenHashes) (firstHash : drop k tokenHashes))
  0
 where
  tokenHashes = map Token.ord (getTokens s)
  base        = Token.ord (maxBound :: Token)
  bases       = map (base ^) [k - 1, k - 2 .. 0]
  hashWithPrev prevTokenHash newTokenHash prevHash =
    dupe $ (prevHash - prevTokenHash * base ^ (k - 1)) * base + newTokenHash
  firstHash = sum (zipWith (*) bases (take k tokenHashes))

fingerprint :: Int -> Int -> String -> [(Int, Int)]
fingerprint w k = map minimum . chunksOf w . (`zip` [0 ..]) . hashes k
