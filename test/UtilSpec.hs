{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module UtilSpec (spec) where

import Import hiding (set)
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import Prelude(putStrLn,print)
import Numeric(showHex)
import           Data.Text (Text,unpack,splitOn,pack)

-- import Network.TLS
-- import Network.TLS.Extra.Cipher
-- import Data.X509.CertificateStore
-- import Data.Default.Class (def)
import Database.Redis

-- import Network.TLS
-- import Network.TLS.Extra.Cipher
-- import Data.X509.CertificateStore
-- import Data.Default.Class (def)
import Database.Redis

spec :: Spec
spec = runIO $ do


  putStrLn $ bin2Hex $ word32ToLeWord8 4

  let cc = reverseHash "139093f00ab659610acac398d44af20d21f5d06ecd6ac6db692e3b81197c2061"
  putStrLn cc

  putStrLn $ show $ hex2Bin "3a32535b915a4a9d023c31faec6b0f6080c96084e8c0135baef15d03a2302600"
  putStrLn $ show $ bin2Hex [132,99,100,3,4,90]

  vv <- liftIO $ randomWord32
  putStrLn $ show vv
  let vvhx = word32ToHexStr vv
  putStrLn $ show vvhx

  -- let d = showHex 1553505796 ""
  -- putStrLn $ d
  redisTest
  return ()
  -- describe "plus2" $ do
    -- it "basic check" $ plus2 0 `shouldBe` 2
    -- it "overflow" $ plus2 maxBound `shouldBe` minBound + 1
    -- prop "minus 2" $ \i -> plus2 i - 2 `shouldBe` i


redisTest :: IO ()
redisTest = do 
  -- (Just certStore) <- readCertificateStore "azure-redis.crt"
  -- let tlsParams = (defaultParamsClient "foobar.redis.cache.windows.net" "") { clientSupported = def { supportedCiphers = ciphersuite_strong }, clientShared = def { sharedCAStore = certStore } }
  let redisConnInfo = defaultConnectInfo { connectHost = "192.168.1.79", connectPort = PortNumber 6379}
  conn <- connect redisConnInfo
  runSet conn "name" "zeng"

  v <- runGet conn "name"

  putStrLn $ show v


  conn1 <- connect redisConnInfo
  conn2 <- connect redisConnInfo

  conn3 <- checkedConnect redisConnInfo
  putStrLn "begin conn"

  -- threadDelay 10000000
  -- 四个连接，执行完后就释放了？

  runRedis conn $ do
    ss <- set "hello" "hello"
    liftIO $ putStrLn $ show ss

  putStrLn "end conn"
  -- threadDelay 10000000


  runRedis conn1 $ do
    set "world" "world"

  putStrLn "end conn1"
  -- threadDelay 10000000

  runRedis conn2 $ do
   hello <- get "hello1"
   liftIO $ print (hello)

  runRedis conn3 $ do
    world <- get "world"
    liftIO $ print (world)
  -- threadDelay 10000000
  -- disconnect conn
  putStrLn "conn"
  -- threadDelay 5000000

  -- runRedis conn1 $ do
  --   hello <- get "hello"
  --   world <- get "world"
  --   liftIO $ print (hello,world)
  -- threadDelay 10000000
  -- putStrLn "conn1"
  


