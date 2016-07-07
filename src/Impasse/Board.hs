module Impasse.Board where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (groupBy, sortBy, find)
import Data.Ord (comparing)
import Control.Monad (guard)
import Control.Arrow (second)

data Piece = Player
           | Stationary
           | UpArrow
           | DownArrow
           | Minus Bool
           | Goal
           deriving (Eq, Ord)

instance Show Piece where
  show Player = "+"
  show Stationary = "o"
  show UpArrow = "^"
  show DownArrow = "v"
  show (Minus False) = "0"
  show (Minus True) = "-"
  show Goal = "X"

data Direction = MoveUp
               | MoveDown
               | MoveLeft
               | MoveRight
               deriving (Eq, Show)

newtype Board = Board { unBoard :: Array (Int, Int) (Set Piece) }
  deriving (Eq)

instance Show Board where
  show = showBoard

type PiecesInPosition = ((Int, Int), Set Piece)

buildBoard :: [PiecesInPosition] -> Board
buildBoard = Board . accumArray Set.union Set.empty ((1,1), (10, 3))

defaultBoard :: Board
defaultBoard = buildBoard [ ((1, 2), Set.singleton Player)
                          , ((10, 2), Set.singleton Goal)
                          ]

showBoard :: Board -> String
showBoard (Board board) = unlines $ "":rows'
  where items = assocs board
        rows = groupBy (\a b -> comparing (snd . fst) a b == EQ) . sortBy (comparing (snd . fst)) $ items
        rows' = map (concatMap (showPosition . snd)) rows
        showPosition x =
          case Set.maxView x of
            Just (e', rest) -> if Set.null rest then show e' else "@"
            Nothing -> " "

findPlayer :: Board -> Maybe (Int, Int)
findPlayer = fmap fst . find ((== [Player]) . Set.toList . snd) . assocs . unBoard

calcNewPosition :: Direction -> (Int, Int) -> (Int, Int)
calcNewPosition dir (x, y) = (x + offsetX, ((y - 1 + offsetY) `mod` 3) + 1)
  where (offsetX, offsetY) = case dir of
                               MoveUp -> (0,-1)
                               MoveDown -> (0,1)
                               MoveLeft -> (-1,0)
                               MoveRight -> (1,0)

checkValid :: Set Piece -> Bool
checkValid = all f . Set.toList
  where f piece = case piece of
                    Stationary -> False
                    UpArrow -> False
                    DownArrow -> False
                    Minus True -> False
                    _ -> True

stepPieces :: Board -> Board
stepPieces (Board board) = board'
  where board' = buildBoard assocs'
        assocs' = concatMap stepPiecesInPosition (assocs board)

stepPiecesInPosition :: PiecesInPosition -> [PiecesInPosition]
stepPiecesInPosition (position, pieces) = map (second Set.singleton . (\x -> stepSinglePiece (position, x))) $ Set.toList pieces

stepSinglePiece :: ((Int, Int), Piece) -> ((Int, Int), Piece)
stepSinglePiece (position, piece) =
  case piece of
    UpArrow -> (calcNewPosition MoveUp position, piece)
    DownArrow -> (calcNewPosition MoveDown position, piece)
    Minus True -> (position, Minus False)
    Minus False -> (position, Minus True)
    _ -> (position, piece)

step :: Direction -> Board -> Maybe Board
step dir board = do
  player <- findPlayer board
  let newPosition@(x, _) = calcNewPosition dir player
  guard $ x >= 1 && x <= 10
  let (Board board') = if dir == MoveUp || dir == MoveDown then stepPieces board else board
      piecesAtNewPosition = board' ! newPosition
  guard $ checkValid piecesAtNewPosition
  return . Board $ board' // [(player, Set.delete Player (board' ! player)), (newPosition, Set.insert Player piecesAtNewPosition)]
