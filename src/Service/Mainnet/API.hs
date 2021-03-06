{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Mainnet.API where

import RIO
import Data.Aeson hiding (Options)
import Data.Default
import Control.Lens hiding(lens,set,Lens')

import Util

data Config = Config {
  _httpPort :: !Int,
  _zcashPort :: !Int,
  _zcashUser :: !String,
  _zcashPassword :: !String,
  _zcashHost :: !String,
  _updateInterval :: !Int
  } deriving (Eq,Show,Generic)
instance ToJSON Config where toJSON = lensyToJSON 1
instance FromJSON Config where parseJSON = lensyParseJSON 1

-- 主网event
data Input = BlockTemplateEvent | SubmitBlockEvent Text
      deriving (Eq,Show)

data Output = ChangeDiff Text | ChangeJob TemplateData
    -- deriving (Eq, Show)

data CoinBase = CoinBase {
  _data :: Text,
  _hash :: Text
  -- _depends :: [Int],
  -- _fee :: Int,
  -- _sigops :: Int,
  -- _foundersreward :: Int,
  -- _required :: Bool
} deriving (Show,Generic)


instance ToJSON CoinBase where toJSON = lensyToJSON 1
instance FromJSON CoinBase where parseJSON = lensyParseJSON 1
instance Default CoinBase where def = CoinBase "" ""

data TemplateData = TemplateData {
  _jobId           :: Maybe Word64,
  _jobIdStr        :: Maybe Text,
  _networkdiff  :: Maybe Double,
  _genTime      :: Maybe Integer,
  _capabilities :: ![Text],
  _version      :: !Word32,
  _previousblockhash :: Text,
  _finalsaplingroothash :: Text,
  _transactions :: [CoinBase],
  _coinbasetxn :: CoinBase,
  _longpollid :: String,
  _target :: Text,
  _mintime :: Int,
  _mutable :: [String],
  _noncerange :: Text,
  _sigoplimit :: Int,
  _sizelimit :: Int,
  _curtime :: Int,
  _bits :: Text,
  _height :: Int,
  _merkleroot :: Maybe Text,
  _nonce :: Maybe Text
} deriving (Show,Generic)



instance ToJSON TemplateData where toJSON = lensyToJSON 1
instance FromJSON TemplateData where parseJSON = lensyParseJSON 1
instance Default TemplateData where def = TemplateData Nothing Nothing Nothing Nothing def def "" "" def def def "" def def "" def def def "" def Nothing Nothing

data BlockTemplate = BlockTemplate {
  _result :: TemplateData,
  _error :: Maybe Text,
  _id :: Int
} deriving (Show,Generic)
instance ToJSON BlockTemplate where toJSON = lensyToJSON 1
instance FromJSON BlockTemplate where parseJSON = lensyParseJSON 1

makeLenses ''TemplateData