module Fingerprint
  ( fingerprint
  , Hash
  , Position
  , Fingerprint(..)
  ) where

import           Control.Monad.Trans.State      ( evalState
                                                , state
                                                )
import           Data.Char                      ( toLower )
import           Data.List.Split                ( chunksOf )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( mapMaybe )
import           Data.Tuple.Extra               ( dupe )
import qualified Token
import           Token                          ( Token )

data Position = Position Int Int
  deriving (Eq, Ord, Show)

data Fingerprint = Fingerprint
  { name   :: String
  , hashes :: Map Hash (Position, Position)
  }
  deriving Show

type Hash = Integer

chars :: String -> [(Char, Position)]
chars =
  concat
    . zipWith (\i cs -> zipWith (\j c -> (c, Position i j)) [1 ..] cs) [1 ..]
    . lines


-- | /O(n)/ 'tokenize' @s@ returns the list of tokens for the relevant characters in
-- @s@
tokenize :: String -> [(Token, Position)]
tokenize = mapMaybe charPos2TokenPos . chars
 where
  charPos2TokenPos (c, p) = do
    t <- Token.fromChar (toLower c)
    pure (t, p)

-- | /O(n)/ 'rollingHash' @k@ @s@ returns a list of hashes of the @k@-grams in
-- @s@ using the rolling hashing function described in Karp and Rabin
-- https://doi.org/10.1147/rd.312.0249
--
-- Example:
--
-- >>> rollingHash 8 "Hello, world"
-- [(16041305554131,(Position 1 1,Position 1 11)),(9736223691463,(Position 1 2,Position 1 12))]
--
rollingHash :: Int -> String -> [(Hash, (Position, Position))]
rollingHash k s = zip (evalState hashStates 0) hashPositions
 where
  ords  = map (Token.ord . fst) (tokenize s)
  base  = Token.ord maxBound
  bases = map (base ^) [k - 1, k - 2 .. 0]
  hash prevOrd newOrd prevHash =
    dupe $ (prevHash - prevOrd * base ^ (k - 1)) * base + newOrd
  firstHash = sum (zipWith (*) bases (take k ords))
  hashStates =
    traverse state (zipWith hash (0 : ords) (firstHash : drop k ords))
  positions     = map snd (tokenize s)
  hashPositions = zip positions (drop (k - 1) positions)

-- | /O(w * n * log n)/ 'fingerprint' @w@ @k@ @s@ returns the document fingerprint of @s@
-- using the winnowing algorithm described in Schleimer, Wilkerson, and Aiken
-- https://doi.org/10.1145/872757.872770, using windows of size @w@ on the
-- @k@-grams of @s@
fingerprint :: String -> Int -> Int -> String -> Fingerprint
fingerprint n w k =
  Fingerprint n . Map.fromList . map minimum . chunksOf w . rollingHash k
