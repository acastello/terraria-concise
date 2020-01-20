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
        return ()
        addingOrModifyingItem id "item" (either return (return . sampleItemByID))
        addingOrModifyingItem id "item" (either return (return . sampleItemByID))
          % (`shouldBe` sampleItemByID 1)

sampleItemByID :: Int -> Item
sampleItemByID iID = Item iID "item" (StaticResource "example") Nothing

infixr 0 %
(%) :: (MonadTrans t, Monad (t m), Monad m) => t m a -> (a -> m b) -> t m b
op % cmp = do r <- op
              lift (cmp r)
