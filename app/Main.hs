{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where

import Import
import Run
import RIO.Process
import Control.Lens ((^?))
import Options.Applicative.Simple hiding (Success)
import Data.Aeson (Result(..), fromJSON)
import Data.Aeson.Lens (key)
import qualified Data.Yaml as Y

import qualified Paths_hpool
import qualified DB



main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_hpool.version)
    "猫池zcash Server"
    "接受矿机连接，统计算力，提交结果到主链"
    (Options
       <$> option auto ( long "port"
                      <> short 'p'
                      <> help "Listen port")
       
       <*> strOption ( long "file"
                      <> short 'f'
                      <> help "config file")
    )
    empty
  vconfig <- Y.decodeFileEither (optionsConfigPath options) >>= \case
    Left e -> do
            throwString ("Error loading config file: " ++ show e)
    Right v -> return v
  let Success config = fromJSON vconfig
      Success mainnetc = fromJSONMaybe $ vconfig ^? key "mainnet"
  lo <- logOptionsHandle stderr (_verbose config)
  pc <- mkDefaultProcessContext
  conn <- DB.connect (_redisHost config) (_redisPort config)
  withLogFunc lo $ \lf -> do
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appConfig = config
          , mainnetConfig = mainnetc
          , redisConn = conn
          }
    runRIO app (run $ optionsPort options)


fromJSONMaybe Nothing = Error "no value"
fromJSONMaybe (Just v) = fromJSON v