{-# LANGUAGE OverloadedStrings #-}

module Gamepedia.CSS where

import Prelude hiding (div, (**))

import Clay as C
import qualified Clay.Flexbox as Flex
import Clay.Render (htmlInline)

import Control.Monad
import Control.Monad.Trans.Writer as W

import Data.Text.Lazy (unpack)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

myCss :: Css
myCss = do
  body ? do
    important $ margin 0 0 0 (px 0)
    display flex
    backgroundColor "#202020"
    star ? do
      textShadow (em 0.05) (em 0.05) (em 0.15) "#202020"
      fontFamily [] [sansSerif]
      color "#b0b1b0"

  div # "#wrapper" ? do
    backgroundColor "#202020"
    display block

  div # ".content" ? do
    border outset 2 $ myBlacks 24
    color "#b0b1b0"
    backgroundColor $ myBlacks 19
    display flex
    flexDirection column
    margin 10 10 10 (px 10)
    padding 8 8 8 (px 8)
    maxWidth (px 640)

  h1 ? do
    fontSize (em 1.5)
    padding (em 0.1) (em 0.1) (em 0.1) (em 0.1)
    margin 0 0 0 (px 0)

  p ? do
    fontSize (em 0.9)
    margin 2 0 2 (px 0)
    textAlign justify

  div # ".item-small" ? do
    whiteSpace nowrap
    overflow hidden
    fontFamily [] [monospace]
    fontSize (em 1)
    margin 2 2 2 (px 2)
    padding 1 1 1 (px 1)
    img ? do
      verticalAlign middle
      padding 1 1 1 (px 1)
      "image-render" -: "pixelated"
    C.span # ".item-small-tooltip" ? do
      visibility hidden
      fontSize (em 0.9)
      position absolute
      zIndex 1
      textAlign center
    hover & C.span # ".item-small-tooltip" ? do
      visibility visible

  table # ".Craft" ? do
    Flex.flex 0 0 none
    backgroundColor $ myBlacks 15
    margin 2 0 2 (px 0)
    padding 2 2 2 (px 2)
    tbody ? do
      tr ? do
        nthChild "odd" & do
          backgroundColor $ myBlacks 27
          div # ".item-small" ? do
            backgroundColor $ myBlacks 37
        nthChild "even" & do
          backgroundColor $ myBlacks 22
          div # ".item-small" ? do
            backgroundColor $ myBlacks 30
        td ? do
          ul ? do
            flexDirection column
            listStyleType none
            paddingLeft (px 0)
            margin (px 0) 0 0 0
  table # ".build-tree" ? do
    Flex.flex 0 0 none
    backgroundColor $ myBlacks 15
    margin 2 0 2 (px 0)
    padding 2 2 2 (px 2)
    tbody ? do
      tr ? do
        nthChild "odd" & do
          backgroundColor $ myBlacks 27
          div # ".item-small" ? do
            backgroundColor $ myBlacks 37
        nthChild "even" & do
          backgroundColor $ myBlacks 22
          div # ".item-small" ? do
            backgroundColor $ myBlacks 30
        td ? do
          ul ? do
            flexDirection column
            listStyleType none
            paddingLeft (px 0)
            margin (px 0) 0 0 0


(***) :: Selector -> Selector -> Selector
parent *** children = (<*>) (<>) (C.** children) parent

-- | Render directly into String for use with Blaze
render :: Css -> String
render = unpack . C.render

-- | Code CSS directly into Blaze HTML elements' 'A.style' attributes
css :: Css -> H.Attribute
css = A.style . H.toValue . C.renderWith htmlInline []

myBlacks :: Float -> Color
myBlacks = hsl 200 18
