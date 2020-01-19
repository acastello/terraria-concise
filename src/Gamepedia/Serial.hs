{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Gamepedia.Serial where

import Codec.Compression.GZip

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State (runStateT)

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL hiding (ByteString)
import Data.Graph.Inductive as G (empty)
import Data.Serialize

import GHC.Generics (Generic)

import Gamepedia.Core

-- | Settings for tuning
type CompressionConfig = (CompressParams, DecompressParams)

data SerialContent = SerialContent
  { serialcontent_version :: ByteString
  , serialcontent_config  :: Maybe CompressionConfig
  , serialcontent_runtime :: Runtime
  } deriving (Show, Generic)

instance Serialize SerialContent where
  put SerialContent{..} = do
    put serialcontent_version
    put serialcontent_config
    maybe
      (put serialcontent_runtime)
      (\(cConfig, _) ->
        putLazyByteString $ compressWith cConfig
        $ encodeLazy $ serialcontent_runtime)
      serialcontent_config
  get = do
    version <- get
    mconfig <- get
    liftM (SerialContent version mconfig) $
      maybe
        get
        (\(_, dConfig) -> do
          either fail return . runGetLazy get . decompressWith dConfig =<< get)
        mconfig

-- | GZip's defaults
defaultCompressionConfig :: CompressionConfig
defaultCompressionConfig = (defaultCompressParams, defaultDecompressParams)

-- | Soft and fast compression
bestSpeedConfig :: CompressionConfig
bestSpeedConfig =
  (defaultCompressParams { compressLevel = bestSpeed }, defaultDecompressParams)

-- | Hard and efficient compression
bestCompressionConfig :: CompressionConfig
bestCompressionConfig =
  (defaultCompressParams { compressLevel = bestCompression }, defaultDecompressParams)

-- Host-aware transformer
type N = ReaderT String

-- | load, use and save state
runZ :: RT IO a -> IO a
runZ op = do
  (val, rt) <- runStateT op =<< load
  save rt
  return val

runZ' :: RT IO a -> IO a
runZ' op = do
  (val, rt) <- runStateT op =<< load
  saveWith bestCompressionConfig rt
  return val

-- | Serialize and save a page to file
save :: Runtime -> IO ()

-- | Serialize and save compressed
save' :: Runtime -> IO ()

-- | Serialize with compression settings
saveWith :: CompressionConfig -> Runtime -> IO ()

(save, save', saveWith) =
  ( write . packT Nothing
  , saveWith bestCompressionConfig
  , \c -> write . packT (Just c)
  ) where
    packT conf = encode . SerialContent "alpha" conf
    write = BS.writeFile "terraria.raw"

-- | Write an empty runtime to get directory started
writeNewRuntime :: IO ()
writeNewRuntime = save (Accums 0 mempty, Terraria G.empty mempty)

-- | load contents and deserialize a page
load :: IO Runtime
load = either error serialcontent_runtime
        . decode <$!> BS.readFile "terraria.raw"

-- necessary instances
deriving instance Generic CompressParams
deriving instance Generic DecompressParams
instance Serialize CompressionLevel
instance Serialize WindowBits
instance Serialize CompressParams
instance Serialize Method
instance Serialize MemoryLevel
instance Serialize CompressionStrategy
instance Serialize DecompressParams

