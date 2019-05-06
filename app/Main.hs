{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
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
  config <- Y.decodeFileEither (optionsConfigPath options) >>= \case
    Left e -> do
            throwString ("Error loading config file: " ++ show e)
    Right v -> return v
  lo <- logOptionsHandle stderr (_verbose config)
  pc <- mkDefaultProcessContext
  conn <- DB.connect (_redisHost config) (_redisPort config)
  withLogFunc lo $ \lf -> do
    eventChan <- newChan
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appConfig = config
          , appEventChan = eventChan
          , redisConn = conn
          }
    runRIO app (run $ optionsPort options)