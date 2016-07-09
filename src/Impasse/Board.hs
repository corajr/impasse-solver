{-# LANGUAGE DeriveGeneric #-}
module Impasse.Board where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import Text.Read (readPrec)
import Data.Hashable (Hashable)
import Data.List (groupBy, sortBy, find, foldl')
import Data.Graph.AStar (aStar)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (traceShowId)
import Control.Monad (guard, foldM, (<=<))
import Control.Arrow (second)

data Piece = Player
           | Stationary
           | UpArrow
           | DownArrow
           | Minus Bool
           | Goal
           deriving (Eq, Show, Ord)

data Direction = MoveUp
               | MoveDown
               | MoveLeft
               | MoveRight
               deriving (Eq, Show, Ord, Generic)

instance Hashable Direction

newtype Board = Board { unBoard :: Array (Int, Int) (Set Piece) }
  deriving (Eq)

instance Show Board where
  show = showBoard

type PiecesInPosition = ((Int, Int), Set Piece)

showPiece :: Piece -> String
showPiece Player = "+"
showPiece Stationary = "o"
showPiece UpArrow = "^"
showPiece DownArrow = "v"
showPiece (Minus False) = "0"
showPiece (Minus True) = "-"
showPiece Goal = "X"

readPiece :: Char -> Piece
readPiece '+' = Player
readPiece 'o' = Stationary
readPiece '^' = UpArrow
readPiece 'v' = DownArrow
readPiece '0' = Minus False
readPiece '-' = Minus True
readPiece 'X' = Goal
readPiece err = error $ "Unrecognized piece: " ++ [err]

buildBoard :: [PiecesInPosition] -> Board
buildBoard = Board . accum Set.union (array b [(i, z) | i <- range b])
  where b = ((1,1), (10,3))
        z = Set.empty
-- accumArray Set.union Set.empty ((1,1), (10, 3))

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
            Just (e', rest) -> if Set.null rest then showPiece e' else "@"
            Nothing -> " "

readBoard :: String -> Maybe Board
readBoard input = do
  let rows = lines input
  guard $ length rows == 3
  let items = concatMap (map (\x -> if x == ' ' then Set.empty else Set.singleton (readPiece x))) rows
      indices' = sortBy (comparing snd) (range ((1,1), (10,3)))
  return . buildBoard $ zip indices' items

findPlayer :: Board -> Maybe (Int, Int)
findPlayer = fmap fst . find (Set.member Player . snd) . assocs . unBoard

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

-- | Are the player and the goal in the same place?
isSolved :: Board -> Bool
isSolved board = fromMaybe False $ do
  player <- findPlayer board
  let piecesAtPlace = unBoard board ! player
  return $ Goal `Set.member` piecesAtPlace

tryProposedSolution :: Board -> [Direction] -> Bool
tryProposedSolution board = maybe False isSolved . foldM f board
  where f acc x = step x acc -- (traceShowId acc)

positionFromDirections :: [Direction] -> (Int, Int)
positionFromDirections = foldl' (flip calcNewPosition) (1,2)

validMoves :: Board -> [Direction] -> Bool
validMoves board = isJust . foldM (flip step) board

validMovesFrom :: Board -> [Direction] -> HashSet [Direction]
validMovesFrom board dirs = HashSet.filter (validMoves board) moves
  where moves = HashSet.fromList . map ((dirs ++) . (:[])) $ [MoveUp, MoveDown, MoveRight, MoveLeft]

-- | Takes in a starting 'Board' and returns a list of 'Direction's if it can be solved. Otherwise return Nothing.
solve :: Board -> Maybe [Direction]
solve board = last <$> aStar (validMovesFrom board) dist heur (tryProposedSolution board) []
  where dist _ _ = 1
        heur dirs = let (i, j) = positionFromDirections dirs
                    in (10 - i)^2 + (2 - j)^2

solveInput :: String -> String
solveInput = show . (solve <=< readBoard)
