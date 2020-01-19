module GamepediaSpec.CoreSpec where

import Control.Monad.State as ST
import Control.Monad.Trans

import Data.Default

import Gamepedia.Core

import Test.Hspec

spec :: Spec
spec = do
  describe "getOrCreateItem" $ do
    it "should never allow for duplicate items" $ do
      flip evalStateT def $ do
        return ()
        getOrCreateItem "item" %
          (`shouldBe` Right 1)
  return ()

infixr 0 %
(%) :: (MonadTrans t, Monad (t m), Monad m) => t m a -> (a -> m b) -> t m b
op % cmp = do r <- op
              lift (cmp r)
