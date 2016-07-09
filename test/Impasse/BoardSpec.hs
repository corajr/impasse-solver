{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Impasse.BoardSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Impasse.Board

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Control.Arrow (first)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Array (bounds, (//), elems)
import Data.Maybe (isJust)

newtype Position = Position { unPosition :: (Int, Int) }
  deriving (Eq, Show)

newtype EnemyPiece = EnemyPiece { unEnemy :: Piece }
  deriving (Eq, Show, Generic)

instance Hashable EnemyPiece

newtype PositionPiece = PositionPiece { unPositionPiece :: (Position, HashSet EnemyPiece) }
  deriving (Eq, Show)

instance Arbitrary Position where
  arbitrary = (fmap Position . (,)) <$> choose (1, 10) <*> choose (1, 3)

instance Arbitrary EnemyPiece where
  arbitrary =
    EnemyPiece <$> elements [ Minus False
                            , Minus True
                            , UpArrow
                            , DownArrow
                            , Stationary
                            , UpHorizontal
                            , DownHorizontal
                            , RedX False
                            , RedX True
                            , ReduceCircle
                            ]

instance Arbitrary Piece where
  arbitrary = oneof [ unEnemy <$> arbitrary
                    , elements [ Player
                               , Goal
                               ]
                    ]

instance Arbitrary PositionPiece where
  arbitrary = (fmap PositionPiece . (,)) <$> arbitrary <*> fmap HashSet.fromList arbitrary

instance Arbitrary Board where
  arbitrary = do
    (Position playerPos) <- arbitrary
    (Position goalPos) <- arbitrary
    let player = (playerPos, HashSet.singleton Player)
        goal = (goalPos, HashSet.singleton Goal)
    rest <- arbitrary
    let rest' = map ((\(Position p, enemies) -> (p, HashSet.map unEnemy enemies)) . unPositionPiece) rest
    return . buildBoard $ player:goal:rest'

defaultBoardWith :: [((Int, Int), HashSet Piece)] -> Board
defaultBoardWith xs = Board (unBoard defaultBoard // xs)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readPiece" $
    it "read . show == id" $ property $
      \(p :: Piece) -> (readPiece . head . showPiece) p === p
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
  describe "readBoard" $
    it "reads in a board from a string" $
      readBoard (unlines [ "          "
                         , "+        X"
                         , "          "
                         ]) `shouldBe` Just defaultBoard
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
      step MoveUp defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), HashSet.empty), ((1,1), HashSet.singleton Player)])
      step MoveDown defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), HashSet.empty), ((1,3), HashSet.singleton Player)])
      step MoveRight defaultBoard `shouldBe` Just (defaultBoardWith [((1,2), HashSet.empty), ((2,2), HashSet.singleton Player)])
    it "returns Nothing if the move goes off the left edge" $ do
      step MoveLeft defaultBoard `shouldBe` Nothing
      step MoveLeft (defaultBoardWith [((1,2), HashSet.empty), ((1,1), HashSet.singleton Player)]) `shouldBe` Nothing
      step MoveLeft (defaultBoardWith [((1,2), HashSet.empty), ((1,3), HashSet.singleton Player)]) `shouldBe` Nothing
    it "returns Nothing if the move puts the player with another piece" $
      step MoveRight (defaultBoardWith [((2,2), HashSet.singleton Stationary)]) `shouldBe` Nothing
    context "with a row of pieces" $ do
      let simpleBoard = defaultBoardWith [ ((2,2), HashSet.singleton Stationary)
                                         , ((3,2), HashSet.singleton (Minus False))
                                         , ((4,2), HashSet.singleton UpArrow)
                                         , ((5,2), HashSet.singleton DownArrow)
                                         ]
          simpleBoard2 = defaultBoardWith [ ((1,1), HashSet.singleton Player)
                                          , ((1,2), HashSet.empty)
                                          , ((2,2), HashSet.singleton Stationary)
                                          , ((3,2), HashSet.singleton (Minus True))
                                          , ((4,1), HashSet.singleton UpArrow)
                                          , ((5,3), HashSet.singleton DownArrow)
                                          ]
          simpleBoard3 = defaultBoardWith [ ((1,3), HashSet.singleton Player)
                                          , ((1,2), HashSet.empty)
                                          , ((2,2), HashSet.singleton Stationary)
                                          , ((3,2), HashSet.singleton (Minus False))
                                          , ((4,3), HashSet.singleton UpArrow)
                                          , ((5,1), HashSet.singleton DownArrow)
                                          ]
      it "changes the pieces according to their behavior" $ do
        step MoveUp simpleBoard `shouldBe` Just simpleBoard2
        step MoveUp simpleBoard2 `shouldBe` Just simpleBoard3
    let reduceBoard = defaultBoardWith [ ((2,2), HashSet.singleton ReduceCircle)
                                       , ((3,2), HashSet.singleton (RedX True))]
        reduceBoard2 = defaultBoardWith [ ((1,2), HashSet.empty)
                                        , ((2,2), HashSet.singleton Player)
                                        , ((3,2), HashSet.singleton (RedX False))]
        reduceBoard3 = defaultBoardWith [ ((1,2), HashSet.empty)
                                        , ((3,2), HashSet.fromList [RedX False, Player])
                                        ]
    it "causes red X's to become inactive when a 'reduce' circle is hit" $ do
      step MoveRight reduceBoard `shouldBe` Just reduceBoard2
      step MoveRight reduceBoard2 `shouldBe` Just reduceBoard3
  describe "isSolved" $ do
    it "returns True in a simple instance" $
      isSolved (defaultBoardWith [((1,2), HashSet.empty), ((10, 2), HashSet.fromList [Player, Goal])]) `shouldBe` True
    it "returns True iff the player and goal are in the same location" $ property $
      \board -> let elems' = elems $ unBoard board
                    playerAndGoalTogether = any (\x -> Player `HashSet.member` x && Goal `HashSet.member` x) elems'
                in isSolved board === playerAndGoalTogether
  describe "solve" $ do
    it "solves the default board with just the player and the goal" $
      solve defaultBoard `shouldBe` Just (replicate 9 MoveRight)
    it "solves a board with the player, the goal, and a stationary obstacle" $
      solve (defaultBoardWith [((2,2), HashSet.singleton Stationary)]) `shouldSatisfy` isJust
    it "solves a board with the player, the goal, and a moving obstacle" $
      solve (defaultBoardWith [((2,1), HashSet.singleton DownArrow)]) `shouldSatisfy` isJust
    it "solves a board with the player, the goal, and a reducible obstacle" $
      solve (defaultBoardWith [ ((2,2), HashSet.singleton ReduceCircle)
                              , ((3,2), HashSet.singleton (RedX True))
                              ]) `shouldBe` Just (replicate 9 MoveRight)
    it "solves a board with the player, the goal, and a distant reducible obstacle" $
      solve (defaultBoardWith [ ((1,1), HashSet.singleton ReduceCircle)
                              , ((9,2), HashSet.singleton (RedX True))
                              ]) `shouldSatisfy` isJust
