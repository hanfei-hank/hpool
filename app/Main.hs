{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Main (main) where

import Import
import Run
import Options.Applicative.Simple hiding (Success)

import qualified Paths_hpool



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

  startApp (optionsConfigPath options) $ optionsPort options

