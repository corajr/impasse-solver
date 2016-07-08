module Impasse.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Impasse.Board

import Control.Arrow (first)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (bounds, (//), elems)
import Data.Maybe (isJust)

newtype Position = Position { unPosition :: (Int, Int) }
  deriving (Eq, Show)

newtype PositionPiece = PositionPiece { unPositionPiece :: (Position, Set Piece) }
  deriving (Eq, Show)

instance Arbitrary Position where
  arbitrary = (fmap Position . (,)) <$> choose (1, 10) <*> choose (1, 3)

instance Arbitrary Piece where
  arbitrary = elements [ Minus False
                       , UpArrow
                       , DownArrow
                       , Stationary
                       ]

instance Arbitrary PositionPiece where
  arbitrary = (fmap PositionPiece . (,)) <$> arbitrary <*> arbitrary

instance Arbitrary Board where
  arbitrary = do
    (Position playerPos) <- arbitrary
    (Position goalPos) <- arbitrary
    let player = (playerPos, Set.singleton Player)
        goal = (goalPos, Set.singleton Goal)
    rest <- arbitrary
    return . buildBoard $ player:goal:map (first unPosition . unPositionPiece) rest

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
    it "returns Nothing if the move puts the player with another piece" $
      step MoveRight (defaultBoardWith [((2,2), Set.singleton Stationary)]) `shouldBe` Nothing
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
  describe "isSolved" $ do
    it "returns True in a simple instance" $
      isSolved (defaultBoardWith [((1,2), Set.empty), ((10, 2), Set.fromList [Player, Goal])]) `shouldBe` True
    it "returns True iff the player and goal are in the same location" $ property $
      \board -> let elems' = elems $ unBoard board
                    playerAndGoal = Set.fromList [Player, Goal]
                    playerAndGoalTogether = any (playerAndGoal `Set.isSubsetOf`) elems'
                in isSolved board === playerAndGoalTogether
  describe "solve" $ do
    it "solves the default board with just the player and the goal" $
      solve defaultBoard `shouldBe` Just (replicate 9 MoveRight)
    it "solves a board with the player, the goal, and a stationary obstacle" $
      solve (defaultBoardWith [((2,2), Set.singleton Stationary)]) `shouldSatisfy` isJust
    it "solves a board with the player, the goal, and a moving obstacle" $
      solve (defaultBoardWith [((2,1), Set.singleton DownArrow)]) `shouldSatisfy` isJust
