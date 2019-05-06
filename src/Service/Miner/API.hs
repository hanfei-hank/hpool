module Service.Miner.API where

import Json
import Util
import RIO
import RIO.Process
import Data.Aeson hiding (Options)
import Data.Default
import Control.Lens hiding(lens,set,Lens')
import System.IO.Streams (OutputStream)

type MinerID = String    -- 矿机ID

data MinerNotify =
    MinerSubscribe Id MinerID (OutputStream Request) (MVar Response)
  | AuthClient Id MinerID Text Text (OutputStream Request) (MVar Response)
  | MinerDisconnect Int
  | FindNonce Id MinerID [Text] (MVar Response)
