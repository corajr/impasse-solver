{-# LANGUAGE DeriveGeneric #-}
module Impasse.Board where

import Data.Array
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import GHC.Generics (Generic)
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (groupBy, sortBy, find, foldl', splitAt, intercalate)
import Data.Graph.AStar (aStar)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Debug.Trace (traceShowId)
import Control.Monad (guard, foldM, (<=<))
import Control.Arrow (second, (&&&))

data Piece = Player
           | Stationary
           | UpArrow
           | DownArrow
           | UpHorizontal
           | DownHorizontal
           | RedX Bool
           | ReduceCircle
           | Minus Bool
           | Goal
           deriving (Eq, Show, Ord, Generic)

instance Hashable Piece

data Direction = MoveUp
               | MoveDown
               | MoveLeft
               | MoveRight
               deriving (Eq, Show, Ord, Generic)

instance Hashable Direction

newtype Board = Board { unBoard :: Array (Int, Int) (HashSet Piece) }
  deriving (Eq, Generic)

instance Hashable Board where
  hashWithSalt salt = hashWithSalt salt . assocs . unBoard

instance Show Board where
  show = showBoard

type PiecesInPosition = ((Int, Int), HashSet Piece)

showPiece :: Piece -> String
showPiece Player = "+"
showPiece Stationary = "o"
showPiece UpArrow = "^"
showPiece DownArrow = "v"
showPiece UpHorizontal = "`"
showPiece DownHorizontal = "."
showPiece (Minus False) = "0"
showPiece (Minus True) = "-"
showPiece (RedX True) = "x"
showPiece (RedX False) = "z"
showPiece ReduceCircle = "*"
showPiece Goal = "X"

readPiece :: Char -> Piece
readPiece '+' = Player
readPiece 'o' = Stationary
readPiece '^' = UpArrow
readPiece 'v' = DownArrow
readPiece '`' = UpHorizontal
readPiece '.' = DownHorizontal
readPiece '0' = Minus False
readPiece '-' = Minus True
readPiece 'x' = RedX True
readPiece 'z' = RedX False
readPiece '*' = ReduceCircle
readPiece 'X' = Goal
readPiece err = error $ "Unrecognized piece: " ++ [err]

buildBoard :: [PiecesInPosition] -> Board
buildBoard = Board . accum HashSet.union (array b [(i, z) | i <- range b])
  where b = ((1,1), (10,3))
        z = HashSet.empty
-- accumArray HashSet.union HashSet.empty ((1,1), (10, 3))

defaultBoard :: Board
defaultBoard = buildBoard [ ((1, 2), HashSet.singleton Player)
                          , ((10, 2), HashSet.singleton Goal)
                          ]

setView :: (Eq a, Hashable a) => HashSet a -> Maybe (a, HashSet a)
setView hs
  | HashSet.null hs = Nothing
  | otherwise = fmap (id &&& (`HashSet.delete` hs)) . listToMaybe . HashSet.toList $ hs

showBoard :: Board -> String
showBoard (Board board) = unlines $ "":rows'
  where items = assocs board
        rows = groupBy (\a b -> comparing (snd . fst) a b == EQ) . sortBy (comparing (snd . fst)) $ items
        rows' = map (concatMap (showPosition . snd)) rows
        showPosition x =
          case setView x of
            Just (e', rest) -> if HashSet.null rest then showPiece e' else "@"
            Nothing -> " "

readBoard :: String -> Maybe Board
readBoard input = do
  let rows = lines input
  guard $ length rows == 3
  let items = concatMap (map (\x -> if x == ' ' then HashSet.empty else HashSet.singleton (readPiece x))) rows
      indices' = sortBy (comparing snd) (range ((1,1), (10,3)))
  guard $ length items == 30
  return . buildBoard $ zip indices' items

findPlayer :: Board -> Maybe (Int, Int)
findPlayer = fmap fst . find (HashSet.member Player . snd) . assocs . unBoard

calcNewPosition :: Direction -> (Int, Int) -> (Int, Int)
calcNewPosition dir (x, y) = (x + offsetX, ((y - 1 + offsetY) `mod` 3) + 1)
  where (offsetX, offsetY) = case dir of
                               MoveUp -> (0,-1)
                               MoveDown -> (0,1)
                               MoveLeft -> (-1,0)
                               MoveRight -> (1,0)

checkValid :: HashSet Piece -> Bool
checkValid = all f . HashSet.toList
  where f piece = case piece of
                    Stationary -> False
                    UpArrow -> False
                    DownArrow -> False
                    UpHorizontal -> False
                    DownHorizontal -> False
                    Minus True -> False
                    RedX True -> False
                    _ -> True

stepPieces :: (Int, Int) -> Direction -> Board -> Board
stepPieces position dir (Board board) = board'
  where board' = toggleRedXs position $ buildBoard assocs'
        assocs' = concatMap (stepPiecesInPosition dir) (assocs board)

stepPiecesInPosition :: Direction -> PiecesInPosition -> [PiecesInPosition]
stepPiecesInPosition dir (position, pieces) = map (second HashSet.singleton . (\x -> stepSinglePiece dir (position, x))) $ HashSet.toList pieces

toggleRedXs :: (Int, Int) -> Board -> Board
toggleRedXs newPlayerPos original@(Board board) =
  if ReduceCircle `HashSet.member` (board ! newPlayerPos)
  then buildBoard . map g . assocs . fmap (HashSet.map f) $ board
  else original
  where g (position, pieces) = if position == newPlayerPos
                               then (position, HashSet.delete ReduceCircle pieces)
                               else (position, pieces)
        f piece = case piece of
                    RedX x -> RedX (not x)
                    _ -> piece

horizontal :: Direction -> Bool
horizontal MoveLeft = True
horizontal MoveRight = True
horizontal _ = False

stepSinglePiece :: Direction -> ((Int, Int), Piece) -> ((Int, Int), Piece)
stepSinglePiece dir (position, piece) =
  case piece of
    UpArrow -> if horizontal' then (position, piece) else (calcNewPosition MoveUp position, piece)
    DownArrow -> if horizontal' then (position, piece) else (calcNewPosition MoveDown position, piece)
    UpHorizontal -> if horizontal' then (calcNewPosition MoveUp position, piece) else (position, piece)
    DownHorizontal -> if horizontal' then (calcNewPosition MoveDown position, piece) else (position, piece)
    Minus True -> if horizontal' then (position, piece) else (position, Minus False)
    Minus False -> if horizontal' then (position, piece) else (position, Minus True)
    _ -> (position, piece)
  where horizontal' = horizontal dir

step :: Direction -> Board -> Maybe Board
step dir board = do
  player <- findPlayer board
  let newPosition@(x, _) = calcNewPosition dir player
  guard $ x >= 1 && x <= 10
  let (Board board') = stepPieces newPosition dir board
      piecesAtNewPosition = board' ! newPosition
  guard $ checkValid piecesAtNewPosition
  return . Board $ board' // [(player, HashSet.delete Player (board' ! player)), (newPosition, HashSet.insert Player piecesAtNewPosition)]

-- | Are the player and the goal in the same place?
isSolved :: Board -> Bool
isSolved board = fromMaybe False $ do
  player <- findPlayer board
  let piecesAtPlace = unBoard board ! player
  return $ Goal `HashSet.member` piecesAtPlace

tryProposedSolution :: Board -> [Direction] -> Bool
tryProposedSolution board = maybe False isSolved . foldM (flip step) board

positionFromDirections :: [Direction] -> (Int, Int)
positionFromDirections = foldl' (flip calcNewPosition) (1,2)

validMoves :: Board -> [Direction] -> Bool
validMoves board = isJust . foldM (flip step) board

validMovesFrom :: Board -> [Direction] -> HashSet [Direction]
validMovesFrom board dirs = HashSet.filter (validMoves board) moves
  where moves = HashSet.fromList . map ((dirs ++) . (:[])) $ [MoveUp, MoveDown, MoveRight, MoveLeft]

heuristic :: [Direction] -> Int
heuristic dirs = distTo (10, 2)
  where (i, j) = positionFromDirections dirs
        distTo (x, y) = abs (x - i) + abs (y - j)

-- | Takes in a starting 'Board' and returns a list of 'Direction's if it can be solved. Otherwise return Nothing.
solve :: Board -> Maybe [Direction]
solve board = last <$> aStar (validMovesFrom board) dist heuristic (tryProposedSolution board) []
  where dist _ _ = 1

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs

solveInput :: String -> String
solveInput = render . (solve <=< readBoard)
  where render (Just xs) = unlines . intercalate [""] . splitEvery 3 $ map show xs
        render Nothing = "Could not solve!"
