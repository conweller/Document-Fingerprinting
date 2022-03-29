module Query
  ( getMatches
  , allToAll
  ) where

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Fingerprint

type PositionMap = Map Hash (Position, Position)

-- | /O(m * log (n / m + 1)), m <= n/. 'matches' @f1@ @f2@ returns a list of
-- tuples of starting and ending positions at which a matches have been found
-- between the documents represented by @f1@ and @f2@
getMatches
  :: PositionMap
  -> PositionMap
  -> Map Hash ((Position, Position), (Position, Position))
getMatches = Map.intersectionWith (,)

toMap :: Fingerprint -> PositionMap
toMap = Map.fromList . hashes

allToAll :: Int -> [Fingerprint] -> [[String]]
allToAll _ [] = []
allToAll t (f : fs) =
  ( name f
    : map name (filter ((> t) . Map.size . getMatches (toMap f) . toMap) fs)
    )
    : allToAll t fs
