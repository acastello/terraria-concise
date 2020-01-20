{-# LANGUAGE OverloadedStrings #-}
module GamepediaSpec.CoreSpec where

import Control.Monad.State as ST
import Control.Monad.Trans

import Data.Default
import qualified Data.Graph.Inductive as G

import Lens.Micro as Lens

import Gamepedia.Core

import Test.Hspec

spec :: Spec
spec = do

  describe "addingOrModifyingItem" $ do
    it "should never duplicate items by name" $ do
      flip evalStateT def $ do
        addingOrModifyingItem id "item" (either return (return . sampleItemByID))
          % (`shouldBe` sampleItemByID 1)
        addingOrModifyingItem id "item" (either return (return . sampleItemByID))
          % (`shouldBe` sampleItemByID 1)

  describe "addingItem" $ do
    it "creates an item (by name) if it's not present already" $ do
      flip evalStateT def $ do
        addingItem id "item" (return . sampleItemByID)
          % (`shouldBe` sampleItemByID 1)
        addingItem id "item" (return . sampleItemByID)
          % (`shouldBe` sampleItemByID 1)

    it "doesn't update an existing item" $ do
      flip evalStateT def $ do
        addingItem id "item" (return . sampleItemByID)
        addingItem id "item" (return . otherItemByID)
            % (`shouldBe` sampleItemByID 1)

  describe "Component include" $ do
    it "should add an item to the sources" $ do
      (def & include (sampleItemByID 1)) ^. terraria_sources . to G.nodes
        `shouldBe` [1]

    it "should add listed items" $ do
      (def & include (sampleItemByID <$> [1,2])) ^. terraria_sources . to G.nodes
        `shouldBe` [1,2]

    it "should add a recipes edges" $ do
      let  newSources = (def & include (sampleItemByID <$> [1,2])
                             & include exampleRecipe) ^. terraria_sources
      newSources ^. to G.nodes `shouldNotBe` []
      newSources ^. to G.edges `shouldNotBe` []


sampleItemByID :: Int -> Item
sampleItemByID iID = Item iID ("item" <>  show iID)
                          (StaticResource "example") Nothing

otherItemByID :: Int -> Item
otherItemByID iID = Item iID "otherItem" (StaticResource "otherExample") Nothing

exampleRecipe :: Recipe
exampleRecipe = Recipe [(sampleItemByID 1, 1)]
                       [(sampleItemByID 2, 1)] (sampleItemByID 3)

infixr 0 %
(%) :: (MonadTrans t, Monad (t m), Monad m) => t m a -> (a -> m b) -> t m b
op % cmp = do r <- op
              lift (cmp r)
