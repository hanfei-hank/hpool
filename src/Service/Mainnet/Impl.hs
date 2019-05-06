{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- 与主网相关的接口
-- http服务
-- 主链有新块时调用http服务，http服务再通过socket调用getTemplateData方法
module Service.Mainnet.Impl (
    start,
    encodeSubmitData
    ) where

import           Util
import           Import hiding (Handler)
import           Service.Mainnet.API

import           Data.Aeson.Types
import           Data.Aeson 
import           Network.Wai.Handler.Warp (run)
import           Servant as S
import qualified RIO.Vector as V
import           Network.HTTP.Client hiding (Request)
import           Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import           Data.Text (Text,pack)
import           Data.List ((!!))

start :: Chan MainnetEvent -> (MainnetNotify -> RIO App ()) -> RIO App ()
start chan nh = do
     -- 获取block数据
    _ <- async $ runMainnetService chan nh
    -- 启动http服务器
    _ <- async $ runHttpServer chan
    _ <- async $ blockUpdate chan
    logInfo "mainnet service started!"

blockUpdate :: Chan MainnetEvent -> RIO App ()
blockUpdate chan = do
                Config {..} <- asks appConfig 
                forever $ do
                            writeChan chan BlockTemplateEvent
                            logInfo $ "blockUpdate"
                            threadDelay $ _updateInterval*1000000
                return ()

type AppM = ReaderT HttpCtx Handler

data HttpCtx = HttpCtx {
    mainnetChan :: !(Chan MainnetEvent)
}

type BlockApi = "block" :> Get '[JSON] Value 

blockApi :: S.Proxy BlockApi
blockApi = S.Proxy

runHttpServer :: Chan MainnetEvent -> RIO App ()
runHttpServer chan = do
    -- 获取端口
    config <- asks appConfig
    let htx = HttpCtx chan
    let ctx = EmptyContext
    liftIO $ run (_httpPort config) $ mkApp ctx htx

mkApp :: Context '[] -> HttpCtx -> Application
mkApp cfg ctx = 
  serveWithContext blockApi cfg $
    hoistServerWithContext blockApi (S.Proxy :: S.Proxy '[])
      (flip runReaderT ctx ) server
 

server :: ServerT BlockApi AppM
server = blockHandler

blockHandler :: AppM Value
blockHandler = do
    chan <- asks mainnetChan
    let mn = BlockTemplateEvent
    writeChan chan mn
    return "roger that!"

-----------------------------------------------------------------------------------
-------------------------------Network Interface-----------------------------------
-----------------------------------------------------------------------------------

runMainnetService :: Chan MainnetEvent -> (MainnetNotify -> RIO App ()) -> RIO App ()
runMainnetService eventChan nh = do
    logInfo "starting mainnet services"

    Config {..} <- asks appConfig

    --初始化http client,创建request
    manager <- liftIO $ newManager defaultManagerSettings  
    initialRequest <- parseRequest _zcashHost
    let req = initialRequest { method = "POST", port = _zcashPort }
    let request = applyBasicAuth (C.pack _zcashUser) (C.pack _zcashPassword) req --增加授权

    forever $ do 
        e <- readChan eventChan
        logInfo $ displayShow e
        case e of 
            BlockTemplateEvent -> do
                let requestObject = (object ["method" .= ("getblocktemplate" :: Text), "id" .= (0 :: Int)])
                    newRequest = request { requestBody = RequestBodyLBS $ encode requestObject }
                response <- liftIO $ httpLbs newRequest manager
                logInfo $ "The status code was: " <> ( displayShow $ statusCode $ responseStatus response)
                -- 将结果转换成json
                let jsonData = decodeTemplate $ responseBody response
                logInfo $ displayShow $ responseBody response
                case jsonData of 
                    Nothing -> logInfo $ "decode blocktemplate failed"
                    Just d -> do
                        -- todo:通知其他节点
                        let templateData = _result d
                        -- 设置难度系数
                        --nh $ ChangeDiff (_target templateData)
                        -- 发送任务                                 
                        nh $ ChangeJob templateData
                        -- logInfo $ displayShow $ d
            SubmitBlockEvent myData -> do
                let requestObject = (object ["method" .= ("submitblock" :: Text), "id" .= (0 :: Int),"params" .= Array (V.fromList [String myData])])
                    newRequest = request { requestBody = RequestBodyLBS $ encode requestObject }
                response <- liftIO $ httpLbs newRequest manager
                logInfo $ displayShow $ "submitblock result:" <> responseBody response


decodeTemplate :: BL.ByteString -> Maybe BlockTemplate
decodeTemplate = decode'


-- 组装submit数据 那些数据需要倒序 
-- version|prehash|merkleRoot|finalsaplingroothash|time|nbits|nonce|solution|conbaase|transactions
encodeSubmitData :: Text -> [Text] -> TemplateData -> Text
encodeSubmitData nonceSuf tx templateData = ver <> preHash <> mklRoot <> rootHash <> time <> nbits <> nonceValue <> solution <> transNums <> coinbase <> trans
    where ver = pack $ bin2Hex $ word32ToLeWord8 $ _version templateData
          preHash = reverseHash' . _previousblockhash $ templateData
          mklRoot =  liftMaybe . _merkleroot $ templateData
          rootHash = reverseHash' . _finalsaplingroothash $ templateData --倒序
          time = tx !! 2 --倒序
          nbits = reverseHash' . _bits $ templateData --倒序
          nonceValue = (nonceSuf) <> (tx !! 3)
          solution = tx !! 4 
          coinbase =  _data . _coinbasetxn $ templateData --不倒
          trans = foldl' (<>) "" $ map (_data) (_transactions templateData)
          transLength = length . _transactions $ templateData
          transNums = calculateTrans (transLength + 1)
