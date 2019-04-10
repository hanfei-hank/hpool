{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
module DB.Redis (
    Connection
  , connect
  , dbGet
  , dbSet
  , HasConnection(..)
  , get
  , set
) where

import           RIO hiding (set)
import           RIO.Partial (read)

import qualified Database.Redis as R
import qualified Network.Socket as Net

type Connection = R.Connection

connect :: String -> Word16 -> IO Connection
connect host port = do
  let po = read . show $ port :: Net.PortNumber
  let redisConnInfo = R.defaultConnectInfo { R.connectHost = host, R.connectPort = R.PortNumber po}
  R.checkedConnect redisConnInfo

dbGet :: Connection -> ByteString -> IO (Maybe ByteString)
dbGet conn key = do
    R.runRedis conn $ do
      result <- R.get key
      case result of
               Left _ -> pure Nothing
               Right v -> return v

dbSet :: Connection -> ByteString -> ByteString -> IO String
dbSet conn key val = do
   R.runRedis conn $ do
     result <- R.set key val
     return $ show result

class HasConnection env where
    conn :: env -> Connection

instance HasConnection Connection where
    conn = id

get :: HasConnection env => ByteString -> RIO env (Maybe ByteString)
get key = do
  c <- asks conn
  liftIO $ dbGet c key

set :: HasConnection env => ByteString -> ByteString -> RIO env String
set key val = do
  c <- asks conn
  liftIO $ dbSet c key val
