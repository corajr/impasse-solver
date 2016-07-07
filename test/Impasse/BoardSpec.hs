module Impasse.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Impasse.Board

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (bounds, (//))

defaultBoardWith :: [((Int, Int), Set Piece)] -> Board
defaultBoardWith xs = Board (unBoard defaultBoard // xs)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "defaultBoard" $
    it "represents a 10x3 board" $
      bounds (unBoard defaultBoard) `shouldBe` ((1,1), (10, 3))
  describe "showBoard" $
    it "outputs a board as a string" $
      showBoard defaultBoard `shouldBe` unlines [ ""
                                                , "          "
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
      step MoveUp defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), Set.empty), ((1,1), Set.singleton Player)])
      step MoveDown defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), Set.empty), ((1,3), Set.singleton Player)])
      step MoveRight defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), Set.empty), ((2,2), Set.singleton Player)])
    it "returns Nothing if the move goes off the left edge" $ do
      step MoveLeft defaultBoard `shouldBe` Nothing
      step MoveLeft (defaultBoardWith [((1,2), Set.empty), ((1,1), Set.singleton Player)]) `shouldBe` Nothing
      step MoveLeft (defaultBoardWith [((1,2), Set.empty), ((1,3), Set.singleton Player)]) `shouldBe` Nothing
    context "with a row of pieces" $ do
      let simpleBoard = defaultBoardWith [ ((2,2), Set.singleton Stationary)
                                         , ((3,2), Set.singleton (Minus False))
                                         , ((4,2), Set.singleton UpArrow)
                                         , ((5,2), Set.singleton DownArrow)
                                         ]
          simpleBoard2 = defaultBoardWith [ ((1,1), Set.singleton Player)
                                          , ((1,2), Set.empty)
                                          , ((2,2), Set.singleton Stationary)
                                          , ((3,2), Set.singleton (Minus True))
                                          , ((4,1), Set.singleton UpArrow)
                                          , ((5,3), Set.singleton DownArrow)
                                          ]
          simpleBoard3 = defaultBoardWith [ ((1,3), Set.singleton Player)
                                          , ((1,2), Set.empty)
                                          , ((2,2), Set.singleton Stationary)
                                          , ((3,2), Set.singleton (Minus False))
                                          , ((4,3), Set.singleton UpArrow)
                                          , ((5,1), Set.singleton DownArrow)
                                          ]
      it "changes the pieces according to their behavior" $ do
        step MoveUp simpleBoard `shouldBe` Just simpleBoard2
        step MoveUp simpleBoard2 `shouldBe` Just simpleBoard3
