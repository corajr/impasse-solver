{-# LANGUAGE ScopedTypeVariables #-}
module Impasse.BoardSpec (main, spec, defaultBoardWith) where

import Test.Hspec
import Test.QuickCheck

import Impasse.Board

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array (bounds, (//), elems)

newtype Position = Position (Int, Int)
  deriving (Eq, Show)

newtype EnemyPiece = EnemyPiece { unEnemy :: Piece }
  deriving (Eq, Ord, Show)

newtype PositionPiece = PositionPiece { unPositionPiece :: (Position, Set EnemyPiece) }
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
  arbitrary = (fmap PositionPiece . (,)) <$> arbitrary <*> arbitrary

instance Arbitrary Board where
  arbitrary = do
    (Position playerPos) <- arbitrary
    (Position goalPos) <- arbitrary
    let player = (playerPos, Set.singleton Player)
        goal = (goalPos, Set.singleton Goal)
    rest <- arbitrary
    let rest' = map ((\(Position p, enemies) -> (p, Set.map unEnemy enemies)) . unPositionPiece) rest
    return . buildBoard $ player:goal:rest'

defaultBoardWith :: [((Int, Int), Set Piece)] -> Board
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
    let reduceBoard = defaultBoardWith [ ((2,2), Set.singleton ReduceCircle)
                                       , ((3,2), Set.singleton (RedX True))]
        reduceBoard2 = defaultBoardWith [ ((1,2), Set.empty)
                                        , ((2,2), Set.singleton Player)
                                        , ((3,2), Set.singleton (RedX False))]
        reduceBoard3 = defaultBoardWith [ ((1,2), Set.empty)
                                        , ((3,2), Set.fromList [RedX False, Player])
                                        ]
    it "causes red X's to become inactive when a 'reduce' circle is hit" $ do
      step MoveRight reduceBoard `shouldBe` Just reduceBoard2
      step MoveRight reduceBoard2 `shouldBe` Just reduceBoard3
  describe "isSolved" $ do
    it "returns True in a simple instance" $
      isSolved (defaultBoardWith [((1,2), Set.empty), ((10, 2), Set.fromList [Player, Goal])]) `shouldBe` True
    it "returns True iff the player and goal are in the same location" $ property $
      \board -> let elems' = elems $ unBoard board
                    playerAndGoalTogether = any (\x -> Player `Set.member` x && Goal `Set.member` x) elems'
                in isSolved board === playerAndGoalTogether
