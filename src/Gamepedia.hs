{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gamepedia (
  module Gamepedia.Core
, module Gamepedia.Scrape
, module Gamepedia
, renderHtml
) where

import Prelude hiding (div)

import qualified Clay as C

import Control.Monad
import Control.Monad.State as ST

import Data.ByteString.Char8 as BS
import Data.Graph.Inductive as G
import Data.List as L
import Data.Tree as T

import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Gamepedia.CSS
import Gamepedia.Core
import Gamepedia.Scrape
import Gamepedia.Serial

itemTree :: Runtime -> Int -> Html
itemTree runtime iID = html $ do
  let (it, ascs, descs) = evalState (itemContext iID) runtime
  H.head $ do
    H.style $ do
      toMarkup $ render myCss
  div ! A.id "wrapper" $
    div ! A.class_ "content" $
      body $ do
        table ! class_ "build-tree" $ do
          tbody $ do
            forM_ ((L.reverse $ T.levels `foldMap` ascs)
                   <> [[it]] <> (T.levels `foldMap` descs)) $ \its -> do
              tr $ do
                forM_ its (td . itemToMarkup)

page :: Page -> Html
page Page{..} = html $ do
  H.head $ do
    H.style $ do
      toMarkup $ render myCss
  div ! A.id "wrapper" $
    div ! A.class_ "content" $
      body $ do
        h1 $
          toMarkup _page_title
        p $ toMarkup _page_intro
        forM_ _page_recipes recipeTable
        p $ toMarkup _page_notes

recipeTable :: [Recipe] -> Html
recipeTable recipes = table ! class_ "Craft" $ do
  thead $ do th' "Result"
             th' "Ingredients"
             th' "Crafting Station"
  tbody $ forM_ recipes $ \Recipe{..} -> do
    tr $ do
      td $
        oneOrMany (quantifiedToMarkup <$> _recipe_result)
      td $
        oneOrMany (quantifiedToMarkup <$> _recipe_ingredients)
      td $
        oneOrMany [itemToMarkup _recipe_station]


quantifiedToMarkup :: Quantified Item -> Html
quantifiedToMarkup (Item{..}, n) = do
  H.div ! A.class_ "item-small" $ do
    toMarkup n
    img ! src (toValue $ resourceURI _item_sprite)
    toMarkup _item_name

itemToMarkup :: Item -> Html
itemToMarkup Item{..} = do
  H.div ! A.class_ "item-small" $ do
   H.span ! class_ "item-small-tooltip" $ do
     "comes from somewhere"
   img ! src (toValue $ resourceURI _item_sprite)
   toMarkup _item_name

th' :: Html -> Html
th' = th ! monospaced

oneOrMany :: [Html] -> Html
oneOrMany [t] = a t
oneOrMany ts = ul $ forM_ ts li

monospaced :: Attribute
monospaced = css (C.fontFamily [] [C.monospace])
