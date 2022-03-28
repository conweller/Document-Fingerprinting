module Query
  ( matches
  ) where

import qualified Data.Map                      as Map
import           Fingerprint


matches
  :: Fingerprint
  -> Fingerprint
  -> [((Position, Position), (Position, Position))]
matches f1 f2 = Map.elems $ Map.intersectionWith (,) (hashes f1) (hashes f2)
