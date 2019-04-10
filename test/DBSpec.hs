
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DBSpec (spec) where

import RIO
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified DB

spec :: Spec
spec = describe "db set and get" $ do
  it "should equal" $ do
    conn <- DB.connect "127.0.0.1" 6379
    runRIO conn $ do
      DB.set "aa" "aa"
      DB.get "aa"
    `shouldReturn` Just "aa"