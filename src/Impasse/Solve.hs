module Impasse.Solve where

import Impasse.Board
import Control.Monad ((<=<))
import Data.List (foldl', intercalate)
import Data.Maybe (mapMaybe)
import Data.Hashable (Hashable, hashWithSalt, hashUsing)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.Graph.AStar (aStar)

newtype BoardWithMoves = BoardWithMoves { unBoardWithMoves :: (Board, [Direction]) }

instance Eq BoardWithMoves where
  (BoardWithMoves (a, _)) == (BoardWithMoves (b, _)) = a == b

instance Ord BoardWithMoves where
  compare (BoardWithMoves (a, _)) (BoardWithMoves (b, _)) = compare a b

instance Hashable BoardWithMoves where
  hashWithSalt = hashUsing (fst . unBoardWithMoves)

tryProposedSolution :: BoardWithMoves -> Bool
tryProposedSolution (BoardWithMoves (board, _)) = isSolved board

positionFromDirections :: [Direction] -> (Int, Int)
positionFromDirections = foldl' (flip calcNewPosition) (1,2)

validMove :: BoardWithMoves -> Direction -> Maybe BoardWithMoves
validMove (BoardWithMoves (board, dirs)) newDir = do
  nextBoard <- step newDir board
  return $ BoardWithMoves (nextBoard, dirs ++ [newDir])

validMovesFrom :: BoardWithMoves -> HashSet BoardWithMoves
validMovesFrom board = HashSet.fromList moves
  where moves = mapMaybe (validMove board) [MoveUp, MoveDown, MoveRight, MoveLeft]

heuristic :: BoardWithMoves -> Int
heuristic (BoardWithMoves (_, dirs)) = distTo (10, 2)
  where (i, j) = positionFromDirections dirs
        distTo (x, y) = abs (x - i) + abs (y - j)

-- | Takes in a starting 'Board' and returns a list of 'Direction's if it can be solved. Otherwise return Nothing.
solve :: Board -> Maybe [Direction]
solve board = (snd . unBoardWithMoves . last) <$> aStar validMovesFrom dist heuristic tryProposedSolution (BoardWithMoves (board, []))
  where dist _ _ = 1

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

solveInput :: String -> String
solveInput = render . (solve <=< readBoard)
  where render (Just xs) = unlines . intercalate [""] . splitEvery 3 $ map show xs
        render Nothing = "Could not solve!"

