module Impasse.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Impasse.Board

import qualified Data.Set as Set
import Data.Array (bounds, (//))

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "defaultBoard" $
    it "represents a 10x3 board" $
      bounds defaultBoard `shouldBe` ((1,1), (10, 3))
  describe "showBoard" $
    it "outputs a board as a string" $
      showBoard defaultBoard `shouldBe` unlines [ "          "
                                                , "+        X"
                                                , "          "
                                                ]
  describe "findPlayer" $
    it "returns the location of the player" $
      findPlayer defaultBoard `shouldBe` Just (1,2)
  describe "calcNewPosition" $ do
    it "returns the updated position after moving left or right" $ do
      calcNewPosition MoveLeft (2,1) `shouldBe` (1,1)
      calcNewPosition MoveRight (5,3) `shouldBe` (6,3)
    it "wraps around vertically" $ do
      calcNewPosition MoveUp (2,1) `shouldBe` (2,3)
      calcNewPosition MoveDown (5,3) `shouldBe` (5,1)
  describe "step" $ do
    it "returns a new board where the player moves by 1 space" $ do
      step MoveUp defaultBoard `shouldBe` Just (defaultBoard // [((1,2), Set.empty), ((1,1), Set.singleton Player)])
      step MoveDown defaultBoard `shouldBe` Just (defaultBoard // [((1,2), Set.empty), ((1,3), Set.singleton Player)])
      step MoveRight defaultBoard `shouldBe` Just (defaultBoard // [((1,2), Set.empty), ((2,2), Set.singleton Player)])
    it "returns Nothing if the move goes off the left edge" $ do
      step MoveLeft defaultBoard `shouldBe` Nothing
      step MoveLeft (defaultBoard // [((1,2), Set.empty), ((1,1), Set.singleton Player)]) `shouldBe` Nothing
      step MoveLeft (defaultBoard // [((1,2), Set.empty), ((1,3), Set.singleton Player)]) `shouldBe` Nothing

