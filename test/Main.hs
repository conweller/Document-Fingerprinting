module Main
  ( main
  ) where


import           Fingerprint                    ( Fingerprint(..)
                                                , Hash
                                                , Position(..)
                                                , fingerprint
                                                , rollingHash
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , hspec
                                                , it
                                                )
import qualified Token
import           Token                          ( Token(..) )

bruteForceHash :: [Token] -> Hash
bruteForceHash ts = sum $ zipWith
  (*)
  (map (Token.ord maxBound ^) [k - 1 :: Int, k - 2 .. 0])
  (map Token.ord ts)
  where k = length ts

rollingHashSpec :: Spec
rollingHashSpec = describe "rollingHash" $ do
  it "hashes the first k characters correctly" $ do
    fst (head (rollingHash 5 "hello, world"))
      == bruteForceHash [T_H, T_E, T_L, T_L, T_O]
  it "hashes the subsequent k-gram correctly" $ do
    fst (rollingHash 5 "hello, world" !! 1)
      == bruteForceHash [T_E, T_L, T_L, T_O, T_W]
  it "maintains positional information" $ do
    snd (rollingHash 5 "hello,\nworld" !! 1) == (Position 1 2, Position 2 1)

fingerprintSpec :: Spec
fingerprintSpec = describe "fingerprint" $ do
  it "select the minimum hash within a given window" $ do
    let s  = "hello, world"
    let w  = 4
    let k  = 5
    let hs = rollingHash k s
    hashes (fingerprint "" w k s) !! 1 == minimum (take w (drop w hs))
  it "assigns the document name" $ do
    name (fingerprint "fname" 4 4 "hello, world") == "fname"

main :: IO ()
main = hspec $ do
  rollingHashSpec
  fingerprintSpec
