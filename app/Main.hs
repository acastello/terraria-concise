module Main where

import Data.Graph.Inductive as G
import Data.Graph.Inductive.Dot
import Gamepedia.Core
import Gamepedia.Serial

main :: IO ()
main = do
  putStrLn . showDot . fglToDot
    . emap (_item_name . _source_station)
    . nmap ((\i -> show (_item_id i) <> " " <> _item_name i))
    . _terraria_sources . snd =<< load
