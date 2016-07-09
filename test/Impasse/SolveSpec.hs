module Impasse.SolveSpec (main, spec) where

import Impasse.BoardSpec (defaultBoardWith)

import Impasse.Board
import Impasse.Solve

import Test.Hspec

import qualified Data.Set as Set
import Data.Maybe (isJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solve" $ do
    it "solves the default board with just the player and the goal" $
      solve defaultBoard `shouldBe` Just (replicate 9 MoveRight)
    it "solves a board with the player, the goal, and a stationary obstacle" $
      solve (defaultBoardWith [((2,2), Set.singleton Stationary)]) `shouldSatisfy` isJust
    it "solves a board with the player, the goal, and a moving obstacle" $
      solve (defaultBoardWith [((2,1), Set.singleton DownArrow)]) `shouldSatisfy` isJust
    it "solves a board with the player, the goal, and a reducible obstacle" $
      solve (defaultBoardWith [ ((2,2), Set.singleton ReduceCircle)
                              , ((3,2), Set.singleton (RedX True))
                              ]) `shouldBe` Just (replicate 9 MoveRight)
    it "solves a board with the player, the goal, and a distant reducible obstacle" $
      solve (defaultBoardWith [ ((1,1), Set.singleton ReduceCircle)
                              , ((9,2), Set.singleton (RedX True))
                              ]) `shouldSatisfy` isJust

