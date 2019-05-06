module MinerServer (
    start,
    pattern PDifficulty,
    pattern PNotify
    ) where

import           Types
import           Import
import           Json
import           Util    
import           Data.Aeson 
import qualified System.IO.Streams.TCP as TCP
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Concurrent as Streams
import           Data.Connection
import           Network.Socket(PortNumber)




start :: PortNumber -> (MinerNotify -> RIO App ()) -> RIO App ()
start port nh = do
    sock <- liftIO $ TCP.bindAndListen 2 port
    _ <- async $ tcpServer sock $ minerServer nh
    logInfo $ "Starting tcp server on port " <> displayShow port




-- 矿工上送的消息
pattern MSubscribe :: Id -> Text -> Request
pattern MSubscribe i sessionid <- Request "mining.subscribe" (fromJSON @[Text] -> Success [_, sessionid, _, _]) i

pattern MSubscribeNull :: Id -> Request
pattern MSubscribeNull i <- Request "mining.subscribe" (fromJSON @[Maybe Text] -> Success [_, Nothing, _, _]) i -- sessionId为空的情况

pattern MAuthorize :: Id -> Text -> Text -> Request
pattern MAuthorize i usr pwd <- Request "mining.authorize" (fromJSON @(Text, Text) -> Success (usr, pwd)) i

pattern MSubmit :: Id -> [Text] -> Request
pattern MSubmit i p <- Request "mining.submit" (fromJSON @[Text] -> Success p) i

-- 矿池通知的消息
pattern PDifficulty :: Value -> Request
pattern PDifficulty p = Notif "mining.set_target" p

pattern PNotify ::  Value -> Request
pattern PNotify p = Notif "mining.notify" p



-- miner服务处理逻辑 
minerServer :: (MinerNotify -> RIO App ()) -> TCP.TCPConnection -> RIO App ()
minerServer nh conn = do
    let sockAddr = show . snd $ connExtraInfo conn
    -- 创建任务处理队列
    (ins, outs) <- liftIO $ Streams.makeChanPipe
    let 
        -- 读取线程，负责解析矿工发来的请求
        reqParser = do
            v <- json'
            case fromJSON v of
                Error msg -> fail msg
                Success req -> return req
        reader = connReader reqParser outs conn
    race_ reader $ do
        -- 任务处理线程，处理该矿工相关的所有请求
        let readRequest = do
                mmsg <- liftIO $ Streams.read ins
                case mmsg of
                    Nothing -> throwString "closing connection"
                    Just msg -> return msg

            sendResponse :: ToJSON a => a -> RIO env ()
            sendResponse t = 
                liftIO $ send conn msg
                where msg = (encode t) <> "\n" --增加换行符，否则矿机接收不到消息

        req <- readRequest
        -- 登记矿机
        id <- case req of
            MSubscribe i n -> do
                logInfo $ "register miner :" <> display n
                return i
            MSubscribeNull i -> do
                logInfo $ "register miner : sessionid is null "
                return i
            _ -> throwString "wrong subscribe request"

        go <- do
            --生成新的sessionId
            -- sid <- liftIO $ randomText' 12 
            mret <- newEmptyMVar
            nh $ MinerSubscribe id sockAddr outs mret
            ret <- takeMVar mret
            logInfo $ "mret :" <> displayShow ret
            sendResponse ret
            --sendMainnetEvent $ MainnetEvent "send mainnet event"
            return $ \action -> finally action $ do
                nh $ MinerDisconnect 0  
            
        -- 处理主循环
        go $ forever $ do
                req' <- readRequest
                logDebug $ displayShow $ req'
                case req' of
                    MAuthorize i usr pwd -> do
                        logDebug $ display usr <> " : " <> display pwd
                        mret <- newEmptyMVar
                        nh $ AuthClient i sockAddr usr pwd outs mret
                        ret <- takeMVar mret
                        sendResponse ret
                    -- 难度下发
                    PDifficulty p -> do
                        sendResponse req'
                    -- 任务下发
                    PNotify p -> do
                        sendResponse req'
                    -- 矿工提交结果
                    MSubmit i p -> do
                        --logDebug $ displayShow p
                        mret <- newEmptyMVar
                        nh $ FindNonce i sockAddr p mret
                        ret <- takeMVar mret
                        sendResponse ret

                    _ -> do
                        sendResponse $ (errorResponse 0 "already register!")

