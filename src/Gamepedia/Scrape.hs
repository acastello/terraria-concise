{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Gamepedia.Scrape where

import Control.Applicative as App
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader as R
import Control.Monad.State as S
import Control.Monad.IO.Class

import Data.ByteString.Char8 as BS
import Data.Graph.Inductive as G
import Data.Maybe

import Lens.Micro (_2)
import Lens.Micro.Mtl -- ((.=))
import Network.URI
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

import Text.HTML.Scalpel
import Text.Read (readMaybe)

import Gamepedia.Core
import Gamepedia.Serial

-- Wholistic IO, net-capable, runtime-updating Scraper
type S = ScraperT String (N (RT IO))

getPage :: S Page
getPage = liftM4 Page
    (text $ "h1" @: ["id" @= "firstHeading"])
    (text $ "div" @: [hasClass "mw-content-ltr"] // "p")
    (liftM Just getRecipes <|> return Nothing)
    getNotes

getNotes :: S String
getNotes = chroot ("div" @: [hasClass "mw-parser-output"]) $ do
    inSerial $ do seekNext (text $ "span" @: ["id" @= "Notes"])
                  untilNext (text "h2") $
                    liftM mconcat $ many $ seekNext $ text anySelector

getRecipes :: S [Recipe]
getRecipes = do
  chroots ("div" @: [hasClass "crafts"] // "tbody" // "tr") $
    liftM3 Recipe
      (chroots ("td" @: [hasClass "result"] // "span" @: [hasClass "item-link"]) $
        liftM (,1) $ scrapeItem)
      (chroots ("td" @: [hasClass "ingredients"] // "span" @: [hasClass "item-link"]) $
        liftM (,1) $ scrapeItem)
      (chroot ("td" @: [hasClass "station"] // "span" @: [hasClass "item-link"]) $
        scrapeItem)

-- | Scrape all item info in the page. Overwrite any existing info
scrapeItemPage :: S Item
scrapeItemPage = do
  itName <- text $ "h1" @: ["id" @= "firstHeading"]
  addingOrModifyingItem (lift . lift) itName $ \eith -> do
    let itID = either _item_id id eith
    liftM2 (Item itID itName)
      (downloadImage =<<
          attr "src" ("div" @: fmap hasClass ["section", "images"] // "img"))
      (liftM2 (fmap Just . ItemInfo)
        (text $ "h1" @: ["id" @= "firstHeading"])
        (text $ "div" @: [hasClass "mw-content-ltr"] // "p"))

-- | Scrape an inline item
scrapeItem :: S Item
scrapeItem = do iName <- attr "title" "a"
                addingItem (lift . lift) iName $ \iID -> do
                  img <- downloadImage =<< attr "src" "img"
                  return $ Item iID iName img Nothing

{-# WARNING scrapeItemID "Item IDs are not easily attached to actual items" #-}
-- | Scrape Item ID from item page
scrapeItemID :: S Int
scrapeItemID = chroot ("div" @: [hasClass "section", hasClass "ids"] // "li") $ do
  matches ("a" @: ["title" @= "Item IDs"])
  maybe App.empty return . readMaybe =<< text "b"

parseLink :: String -> S String
parseLink maybeRelURL = do
  topUrl <- lift ask
  maybe App.empty (return . show) $ do
    parseURI maybeRelURL
    <|> liftM2 relativeTo (parseRelativeReference maybeRelURL) (parseURI topUrl)

downloadImage :: String -> S Resource
downloadImage url = do
  evalUrl <- parseLink url
  liftIO $ do request <- parseRequest evalUrl
              mgr <- getGlobalManager
              StaticResource <$!> withResponse request mgr responseBody

-- | scrape with own handling of Runtime
scrapeS :: URL -> S a -> (RT IO) (Maybe a)
scrapeS url scr = runReaderT (scrapeT scr =<< liftIO (downloadURLSpec url)) url

-- | scrape using and updating serialized Runtime
(...) :: Component a => S a -> URL -> IO (Maybe a)
op ... url = runZ $ do mres <- scrapeS url op
                       forM_ mres ((_2 %=) . flip amend)
                       return mres
