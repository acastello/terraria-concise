module GamepediaSpec where

import Data.Graph.Inductive as G

import Test.Hspec

spec :: Spec
spec = do

  describe "Graph &" $ do

    it "appends an element and its edges" $ do
      (([], 1, "first", []) & empty')
        `lab` 1 `shouldBe` Just "first"

    it "updates an element" $ do
      ([], 1, "new", []) & insNode (1, "old") empty'
        `lab` 1 `shouldBe` Just "new"

    it "updates adjacency" $ do
      (fst $ match 1 $
        ([((), 2)], 1, "new", []) & insNodes [(1,"old"), (2, "alt")] empty')
          `shouldBe` Just ([((), 2)], 1, "new", [])

empty' :: Gr String ()
empty' = empty
