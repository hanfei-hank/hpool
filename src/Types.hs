{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}





module Types where

import Json
import Util
import RIO
import RIO.Process
import Data.Aeson hiding (Options)
import Data.Default
import Control.Lens hiding(lens,set,Lens')
import System.IO.Streams (OutputStream)
import Network.Socket (PortNumber)
import qualified DB

import Service.Mainnet.API as Mainnet
import Service.Miner.API as Miner

-- | Command line arguments
data Options = Options
  { 
    optionsPort :: !PortNumber
   ,optionsConfigPath :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext

  , appConfig :: !AppConfig
  , mainnetConfig :: !Mainnet.Config
  -- Add other app-specific configuration information here
  , redisConn :: !DB.Connection -- redis数据库连接
  } deriving (Generic)

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

instance DB.HasConnection App where
  conn = redisConn

-- event 定义
data AppEvent = 
    AEMiner   Miner.Output
  | AEMainNet Mainnet.Output

data PoolStats = PoolStats {  
    _pBlockChangeId   :: Word64,
    _pWorkbaseId      :: Word64,
    _pLastHash        :: Text,
    _pUsers          :: !Int,     -- 账户数量 jkd
    _pClients        :: !Int,     -- 矿机数量 jkd.001 jkd.002　
    _pStartTime      :: !Integer, --　启动时间戳　精确到毫秒
    _pUnaccountedShares  :: !Int64,
    _pAccountedShares  :: !Int64,
    _pUserstatsCycle  :: !Int64,
  
    _pSps1           :: !Double,   -- Shares per second for 1/5/15/60 minute rolling averages
    _pSps5           :: !Double,
    _pSps15          :: !Double,
    _pSps30          :: !Double,
    _pSps60          :: !Double,
  
    _pSsps1          :: !Double,    --Diff shares per second, 1 minute rolling average 
    _pDsps5          :: !Double,    -- 5 minute ... 
    _pDsps15         :: !Double,    -- 15 minute ... 
    _pDsps30         :: !Double,    -- 30 minute ... 
    _pDsps60         :: !Double,
    _pDsps1440       :: !Double,
    _pDsps10080      :: !Double,
    
    _pUnaccountedDiffShares :: !Int64,
    _pAccountedDiffShares   :: !Int64,
    _pUnaccountedRejects    :: !Int64,
    _pAccountedRejects      :: !Int64
  
} deriving (Generic)
  

data Share = Share {
    _shash :: Text,
    _wbid :: !Word64
}deriving (Generic)
  
data MinerAccount = MinerAccount {  
    _aUserName        :: !String,     -- 账户名　
    _aClients         :: !Int64,
    _aBestDiff       :: !Double,
    _aShares         :: !Int64,
    _aSharerej       :: !Int64,
    _aUadiff         :: !Int64,   -- Shares not yet accounted for in hashmeter
  
    _aDsps1          :: !Double,    --Diff shares per second, 1 minute rolling average 
    _aDsps5          :: !Double,    -- 5 minute ... 
    _aDsps15         :: !Double,    -- 15 minute ... 
    _aDsps30         :: !Double,    -- 30 minute ... 
    _aDsps60         :: !Double,
    _aDsps1440       :: !Double,
    _aDsps10080      :: !Double,
    
    _aLastShare      :: !Int64,
    _aLastDecay      :: !Int64,
  
    _aAuthorised     :: !Bool
  
  
} deriving (Generic)
  
  
data MinerClient = MinerClient {  
    _sessionId      :: !String,
    _userName       :: !String,     -- 账户名
    _workerName     :: !String,     -- 矿工名
    _dsps1          :: !Double,    --Diff shares per second, 1 minute rolling average 
    _dsps5          :: !Double,    -- 5 minute ... 
    _dsps15         :: !Double,    -- 15 minute ... 
    _dsps30         :: !Double,    -- 30 minute ... 
    _dsps60         :: !Double,
    _dsps1440       :: !Double,
    _dsps10080      :: !Double,
    _shares         :: !Int64,
    _sharerej       :: !Int64,
    _diff           :: !Double,
    _oldDiff        :: !Double,
    _diffChangeJobId :: !Word64,
    _bestDiff       :: !Double,
    _uadiff         :: !Int64,   -- Shares not yet accounted for in hashmeter
    _ldc            :: !Integer, -- Last diff change
    _ssdc           :: !Int,    --Shares since diff change
    _firstShare     :: !Integer,
    _lastShare      :: !Integer,
    _lastDecay      :: !Integer,
    _startTime      :: !Integer,
    _subscribed     :: !Bool,
    _authorised     :: !Bool,
    _idle           :: !Bool, --超过１分无share，diff为０　超过10分钟无share，标记空闲状态
    _nonce2         :: !String,  --nonce的后４个字节
    _clientOuts     :: !(OutputStream Request) 
  
} deriving (Generic)
    

type AccountID = String
type JobID = Text
type Hash = Text

type MinerMap =  (Map MinerID  MinerClient)
type AccountMap = (Map AccountID MinerAccount)
type JobMap = (Map JobID TemplateData)
type ShareMap = (Map Hash Share)

type MinerPool = IORef MinerMap 
type AccountPool = IORef AccountMap
type JobPool = IORef JobMap
type SharePool = IORef ShareMap
                                                                  

data AppConfig = AppConfig {
  _verbose :: !Bool,
  _minDiff :: !Int,
  _startDiff :: !Int,
  _maxDiff :: !Int,
  _redisHost :: !String,
  _redisPort :: !Word16
  } deriving (Eq,Show,Generic)
instance ToJSON AppConfig where toJSON = lensyToJSON 1
instance FromJSON AppConfig where parseJSON = lensyParseJSON 1




makeLenses ''PoolStats
makeLenses ''MinerClient
makeLenses ''MinerAccount

