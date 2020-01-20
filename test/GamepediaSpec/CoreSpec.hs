{-# LANGUAGE OverloadedStrings #-}
module GamepediaSpec.CoreSpec where

import Control.Monad.State as ST
import Control.Monad.Trans

import Data.Default

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

sampleItemByID :: Int -> Item
sampleItemByID iID = Item iID "item" (StaticResource "example") Nothing

otherItemByID :: Int -> Item
otherItemByID iID = Item iID "item" (StaticResource "otherExample") Nothing

infixr 0 %
(%) :: (MonadTrans t, Monad (t m), Monad m) => t m a -> (a -> m b) -> t m b
op % cmp = do r <- op
              lift (cmp r)
