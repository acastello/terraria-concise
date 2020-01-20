{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Gamepedia.Core (
  ItemInfo(..)
, iteminfo_intro
, iteminfo_notes
, Item(..)
, item_info
, item_sprite
, Quantified
, Recipe(..)
, recipe_result
, recipe_ingredients
, recipe_station
, Page(..)
, page_intro
, page_notes
, page_recipes
, page_title
, Resource(..)
, resourceURI
, Component(..)
, Source(..)
, source_element
, source_npc
, source_rate
, source_station
, source_yield
, Terraria(..)
, Accums(..)
, Runtime
, RT
, itemContext
, addingItem
, addingOrModifyingItem
, debug
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State as ST
import Control.Monad.Trans.Maybe

import Data.Bifunctor
import Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BS64
import Data.Default
import Data.Function (on)
import Data.Graph.Inductive as G
import Data.Graph.Inductive.NodeMap as G
import Data.Map as M
import Data.List as L
import Data.Ratio
import Data.Serialize
import Data.Tree

import GHC.Generics (Generic)

import Lens.Micro as Lens
import Lens.Micro.Mtl
import Lens.Micro.TH

-- | Remote or loaded, encoded resource
data Resource =
    StaticResource ByteString
  | RemoteResource String
  deriving (Generic)
instance Serialize Resource
makeLenses ''Resource

instance Show Resource where
  show (StaticResource _) = "<static>"
  show (RemoteResource u) = show u

-- | Encode (static) or reference (remote) a resource
resourceURI :: Resource -> String
resourceURI (StaticResource bs) = "data:image/png;base64,"
    <> (BS.unpack $ BS64.encode bs)
resourceURI (RemoteResource uri) = uri

-- | Info extracted from Item page
data ItemInfo = ItemInfo
  { _iteminfo_intro :: String
  , _iteminfo_notes :: String
  } deriving (Show, Generic)
instance Serialize ItemInfo
makeLenses ''ItemInfo

-- | Model for any non-AI element in the game
data Item = Item
  { _item_id     :: Int -- ^ Internal
  , _item_name   :: String
  , _item_sprite :: Resource
  , _item_info   :: Maybe ItemInfo
  } deriving (Show, Generic)
instance Serialize Item
makeLenses ''Item

instance Eq Item where
  (==) = (==) `on` _item_name

type Quantified item = (item, Int)

-- | TODO
type NPC = ()

type IntRatio = Ratio Int

-- | Comprises all ways to obtain an item
data Source =
    Crafted
      { _source_station :: Item
      , _source_yield   :: IntRatio }
  | Gathered
      { _source_element :: Item
      , _source_yield   :: IntRatio }
  | Dropped
      { _source_npc     :: NPC
      , _source_rate    :: IntRatio }
  deriving (Show, Generic)
instance Serialize Source
makeLenses ''Source

-- | Directed graph showing ways to obtain anything
type SourceGraph = Gr Item Source
instance (Serialize a, Serialize b) => Serialize (Gr a b)

instance Default (G.Gr a b) where
  def = G.empty

-- | Main data structure holding all relevant information
data Terraria = Terraria
  { _terraria_sources :: SourceGraph
  , _terraria_items   :: Map String Item
  } deriving (Show, Generic)
instance Serialize Terraria
instance Default Terraria
makeLenses ''Terraria

-- | Recipe
data Recipe = Recipe
  { _recipe_result      :: [Quantified Item]
  , _recipe_ingredients :: [Quantified Item]
  , _recipe_station     :: Item
  } deriving (Show, Generic)
instance Serialize Recipe
makeLenses ''Recipe

-- | Page
data Page = Page
  { _page_title    :: String
  , _page_intro    :: String
  , _page_recipes  :: Maybe [Recipe]
  , _page_notes    :: String
  } deriving (Show, Generic)
instance Serialize Page
makeLenses ''Page

-- | Merge Runtime
data Accums = Accums
  { _accums_max_id :: Int  -- ^ Max item ID so far
  , _accums_item_ids :: Map String Int  -- ^ Map item name to item ID
  } deriving (Show, Generic)
makeLenses ''Accums
instance Serialize Accums
instance Default Accums

type Runtime = (Accums, Terraria)

type RT m = StateT Runtime m

-- | items amendable to an existing Terraria structure
class Component a where
  amend :: Terraria -> a -> Terraria

instance Component Item where
  amend ter it = ter Lens.& terraria_items %~ M.insert (it ^. item_name) it

instance Component Recipe where
  amend ter Recipe{..} =
    ter Lens.& terraria_sources %~ G.insEdges allDirections
    where
      allDirections = liftM2
          ( \(r, n) (c, m) ->
            ( r ^. item_id
            , c ^. item_id
            , Crafted _recipe_station (n % m)))
          _recipe_ingredients
          _recipe_result

instance Component a => Component [a] where
  amend ter xs = L.foldr (.) id (flip amend <$> xs) ter

-- | Get Item or create its ID
getOrCreateItem :: Monad m => String -> RT m (Either Item Int)
getOrCreateItem iName = do
  runMaybeT $ do
    iID <- MaybeT $ M.lookup iName <$!> use (_1 . accums_item_ids)
    fmap (^._3) $ MaybeT $ fst . match iID <$!> use (_2.terraria_sources)
  >>= \case
    Just it -> return $ Left it
    Nothing  -> do
      iID <- _1 . accums_max_id <%= (+1)
      _1 . accums_item_ids %= M.insert iName iID
      return (Right iID)

addingItem :: (Monad m, Monad n)
  => (forall a. RT m a -> n a) -> String -> (Int -> n Item) -> n Item
addingItem liftF itName constructM =
    addingOrModifyingItem liftF itName (either return constructM)

addingOrModifyingItem :: (Monad m, Monad n)
  => (forall a. RT m a -> n a) -> String -> (Either Item Int -> n Item) -> n Item
addingOrModifyingItem liftF itName constructM = do
  liftF (getOrCreateItem itName) >>= \case
    Left it -> constructM (Left it)
    Right iID -> do it' <- constructM (Right iID)
                    liftF $ addItem it'
                    return it'

-- | add a singleton node
addItem :: Monad m => Item -> RT m ()
addItem it = _2.terraria_sources %= (([], it ^. item_id, it, []) G.&)

-- | Sort an item's components and derivatives to obtain the build trees
-- spanning from it (dfs)
itemContext :: Monad m => Int -> RT m (Item, Forest Item, Forest Item)
itemContext iID = G.match iID <$!> use (_2 . terraria_sources) >>= \case
  (Nothing, _) -> fail "item not found"
  (Just (ascs, _, it, descs), gr) ->
    let (descForest, gr') = foldDescs gr descs
        (ascForest, _) = foldAscs gr' ascs
    in return (it, ascForest, descForest)
  where
    foldAscs                 = foldTowards (\ascs _ -> ascs)
    foldDescs                = foldTowards (\_ descs -> descs)
    foldTowards f gr =
        L.foldr
          (\(_, desc) (stepForest, stepGraph) ->
            case match desc stepGraph of
              (Nothing, _) -> (stepForest, stepGraph)
              (Just (stepAscs, _, it, stepDescs), nextGraph)
                -> first (\children -> Node it children : stepForest)
                    (foldTowards f nextGraph (f stepAscs stepDescs)))
          ([], gr)

-- | write some html
debug :: String -> IO ()
debug t = Prelude.writeFile "debug.html" $ t -- "<html><body>" <> t <> "</body></html>"
