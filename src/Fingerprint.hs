module Fingerprint
  ( fingerprint
  ) where

import           Control.Monad.Trans.State      ( evalState
                                                , state
                                                )
import           Data.Char                      ( toLower )
import           Data.Foldable                  ( minimumBy )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( mapMaybe )
import           Data.Tuple.Extra               ( dupe )
import qualified Token
import           Token                          ( Token )

type Index = Int
type Hash

-- | /O(n)/ 'tokenize' @s@ returns the list of tokens for the relevant characters in
-- @s@
tokenize :: String -> [Token]
tokenize = mapMaybe (Token.fromChar . toLower)

-- | /O(n)/ 'rollingHash' @k@ @s@ returns a list of hashes of the @k@-grams in
-- @s@ using the rolling hashing function described in Karp and Rabin
-- https://doi.org/10.1147/rd.312.0249
rollingHash :: Int -> String -> [Hash]
rollingHash k s = evalState hashStates 0
 where
  tokenHashes = map Token.ord (tokenize s)
  base        = Token.ord (maxBound :: Token)
  bases       = map (base ^) [k - 1, k - 2 .. 0]
  hashWithPrev prevTokenHash newTokenHash prevHash =
    dupe $ (prevHash - prevTokenHash * base ^ (k - 1)) * base + newTokenHash
  firstHash  = sum (zipWith (*) bases (take k tokenHashes))
  hashStates = traverse
    state
    (zipWith hashWithPrev (0 : tokenHashes) (firstHash : drop k tokenHashes))

-- | /O(w * n)/ 'fingerprint' @w@ @k@ @s@ returns the document fingerprint of @s@
-- using the winnowing algorithm described in Schleimer, Wilkerson, and Aiken
-- https://doi.org/10.1145/872757.872770, using windows of size @w@ on the
-- @k@-grams of @s@
fingerprint :: Int -> Int -> String -> [(Hash, Index)]
fingerprint w k =
  map
      (minimumBy
        (\pair1 pair2 -> (negate <$> pair1) `compare` (negate <$> pair2))
      )
    . chunksOf w
    . (`zip` [0 ..])
    . rollingHash k
