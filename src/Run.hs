{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Run (run, Streams.makeChanPipe) where

import           Import
import           Json
import           Util
import qualified RIO.Map as Map
import           Data.Aeson 
import qualified Mainnet
import qualified MinerServer as MS
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Concurrent as Streams
import           Network.Socket(PortNumber)
import qualified Data.ByteString as B
import           Data.Text (Text,unpack,splitOn,pack)
import qualified Data.Text.Encoding as T
import qualified RIO.Vector as V
import           Data.Default
import           Data.List ((!!))
import           Data.Bits
import           Data.Maybe (fromJust)





run :: PortNumber -> RIO App ()
run port = do
    Mainnet.start
    MS.start port
    handleAppEvent


handleAppEvent :: RIO App ()
handleAppEvent = do
    logInfo "starting AppEvent handler"

    rand <- liftIO $ randomWord64
    let jobRand = rand `shiftR` 32
    poolStats <- newIORef (PoolStats jobRand jobRand ""
                                    def def def def def
                                    def def def def def
                                    def def def def def
                                    def def def def def
                                    def def)
                                    
    curJob  <- newIORef (Null :: Value)
    jobIdRef <- newIORef (jobRand :: Word64)
    jobPool :: JobPool <- newIORef mempty
    minerPool :: MinerPool <- newIORef mempty
    accountPool :: AccountPool <- newIORef mempty
    sharePool :: SharePool <- newIORef mempty
    
    let updateJob task = do
            logInfo $ "updateJob:"
            miners <- Map.elems <$> readIORef minerPool
            let resp = Notif "mining.notify" task
            forConcurrently_ miners $ \minerClient -> do
                liftIO $ Streams.write (Just resp) $ _clientOuts minerClient 
        --更新难度值
        -- updateDiff target = do 
        --     miners <- Map.elems <$> readIORef minerPool
        --     let resp = MS.PDifficulty $ toJSON [String target]
        --     forConcurrently_ miners $ \minerClient -> do
        --         liftIO $ Streams.write (Just resp) $ _clientOuts minerClient
        
        --　发送任务
        sendJob cliOuts task = do
            mboxSend cliOuts $ MS.PNotify $ toJSON task                
        -- 发送难度信息
        sendDiff cliOuts ddiff = do
            let targetValue = reverseHash $ targetFromDiff ddiff
            logInfo $ "target:" <> displayShow targetValue
            mboxSend cliOuts $ MS.PDifficulty $ toJSON $ [targetValue]
    
    eventChan <- asks appEventChan
    forever $ do
        e <- readChan eventChan
        case e of
            MinerSubscribe i sessionid cliOuts mret -> do
                
                --加入矿机池
                modifyIORef' minerPool $ Map.insert sessionid $ MinerClient sessionid def def def def def
                                                                            def def def def def def
                                                                            def def def def def 
                                                                            def def def def def 
                                                                            def False False False "" cliOuts
                                                                        
                --打印sessionId  --test
                miners <- Map.keys <$> readIORef minerPool
                logInfo $ "sessionIds:" <> displayShow miners
                
                -- 生成随机 nonce和sessionid
                (sid,nonceValue) <- liftIO $ randomNonce

                updateMinerClient minerPool sessionid nonce2 $ \_->(unpack nonceValue)
                updateMinerClient minerPool sessionid subscribed $ \_->True
         
                let vv = Array (V.fromList [String sid,String nonceValue])
                putMVar mret $ Response  vv i

                job <- readIORef curJob
                sendJob cliOuts job
                -- 发送难度信息
                Config {..} <- asks appConfig
                let ddiff = fromIntegral _startDiff :: Double
                sendDiff cliOuts ddiff
            
            MinerDisconnect n -> do
                logInfo $ "unregister miner " <> display n

            AuthClient i sessionid usr pwd cliOuts mret -> do
                updateMinerClient minerPool sessionid authorised $ \_->True
                stime <- liftIO getMillisTimeStamp
                updateMinerClient minerPool sessionid startTime $ \_->stime
                updateMinerClient minerPool sessionid workerName $ \_-> (unpack usr)

                let username = unpack $ (splitOn "." usr) !! 0
                logInfo $ "username:" <> displayShow username <> " password:" <> displayShow pwd
                updateMinerClient minerPool sessionid userName (\_ -> username)

                updateMinerAccount accountPool username aUserName (\_ -> username)
                updateMinerAccount accountPool username aClients (\c -> c + 1)
                updateMinerAccount accountPool username aAuthorised (\_->True)
                                     
                                     
                putMVar mret $ okResponse i

                Config {..} <- asks appConfig
                let ddiff = fromIntegral _startDiff :: Double
                updateMinerClient minerPool sessionid diff $ \_-> ddiff 
                -- 发送难度信息
                sendDiff cliOuts ddiff

            FindNonce i sessionid n mret -> do
                
                nonce2Value <- getMinerClientField minerPool sessionid nonce2 ""
                username    <- getMinerClientField minerPool sessionid userName ""
                newestJobId <- getMinerClientField minerPool sessionid diffChangeJobId (0::Word64)

                let nonceSuf = pack nonce2Value
                let jobStr = n !! 1

                logInfo $ "nonceSuf: " <> displayShow nonceSuf <> "josId: " <> displayShow jobStr
                jobMap <- readIORef jobPool
                let mbTpdata =  Map.lookup jobStr jobMap
                case mbTpdata of
                    Nothing -> putMVar mret $ errorResponse i "Invalid JobID"
                    Just tpdata ->  do
                        
                        blockChangeId <- getPoolStatsField poolStats pBlockChangeId

                        let jobId' = fromJust $ _jobId tpdata
                        --stale
                        if (jobId' < blockChangeId)
                        then putMVar mret $ errorResponse i "Stale"
                        else do 
                                -- 工作难度   
                                let wdiff = fromJust $ _networkdiff tpdata
                                -- 提交的hash值
                                let hashBin = calBlockHash nonceSuf n tpdata
                                let hashHex = bin2Hex $ B.unpack $ sha256 $ B.pack hashBin
                               　--　提交的难度值
                                let sdiff = diffFromTarget hashHex
                                -- 机器最好的难度　
                                cBestDiff <- getMinerClientField  minerPool sessionid bestDiff 0.0
                                --　账户最好的难度
                                accBestDiff <- getMinerAccountField  accountPool username aBestDiff 0.0
                                logInfo $  "share diff : "  <> displayShow sdiff <> "best diff : "  <> displayShow  cBestDiff
                                
                                when (sdiff > cBestDiff) $ do
                                    updateMinerClient minerPool sessionid bestDiff $ \_->sdiff 
                                when (sdiff > accBestDiff) $ do
                                    updateMinerAccount accountPool username aBestDiff $ \_->sdiff

　　　　　　　　　　　　　　　　　　　-- 当前分配的难度　　　　　　
                                curDiff <- getMinerClientField minerPool sessionid diff 0.0
                                -- 上次分配的难度
                                preDiff <- getMinerClientField minerPool sessionid oldDiff 0.0
                                
                                shareMaps <- readIORef sharePool
                                let mbShare =  Map.lookup (pack hashHex) shareMaps
                                -- 难度符合　可以提交    
                                when (sdiff >= wdiff) $ do 
                                    if (jobId' < newestJobId)
                                    then do
                                        if (sdiff >= preDiff)
                                        then do
                                            case mbShare of  
                                                Nothing ->  do
                                                    --加入share表
                                                    modifyIORef' sharePool $ Map.insert (pack hashHex) (Share ((pack hashHex)) jobId')                                  
                                                    --提交block
                                                    let submitData = Mainnet.encodeSubmitData nonceSuf n tpdata
                                                    sendMainnetEvent $ SubmitBlockEvent submitData
                                                _ -> putMVar mret $ errorResponse i "Duplicate"  
                                        else
                                            putMVar mret $ errorResponse i "Above target" 
                                    else do
                                        if (sdiff >= curDiff)
                                        then do
                                            case mbShare of  
                                                Nothing ->  do
                                                    --加入share表
                                                    modifyIORef' sharePool $ Map.insert (pack hashHex) (Share ((pack hashHex)) jobId')                                  
                                                    --提交block
                                                    let submitData = Mainnet.encodeSubmitData nonceSuf n tpdata
                                                    sendMainnetEvent $ SubmitBlockEvent submitData
                                                _ -> putMVar mret $ errorResponse i "Duplicate"  
                                        else    
                                            putMVar mret $ errorResponse i "Above target"
                                
                                when (sdiff >= preDiff) $
                                    addSubmit poolStats accountPool minerPool sessionid username preDiff True True
                                when (sdiff >= curDiff) $
                                    addSubmit poolStats accountPool minerPool sessionid username curDiff True True    
          
                                putMVar mret $ okResponse i
 
            ChangeJob templateData -> do
                 -- 设置job
                jobId' <- readIORef jobIdRef
                secstime <- liftIO $ getSecsTimeStamp
                
                jobMapTemp <- readIORef jobPool
                --过滤超时的job
                when ((Map.size jobMapTemp) > 2) $ do
                    writeIORef jobPool $  Map.filter (\tpdata -> ((fromJust $ _genTime tpdata) > (secstime - 150)))  jobMapTemp
                 
                let jobStr = pack $ word64ToHexStr jobId'
                
                let netdiff = diffFromTarget $  unpack $ _target templateData
                let templateData1 = templateData {_jobId = (Just jobId'), _jobIdStr = (Just jobStr), _networkdiff = (Just netdiff), _genTime = (Just secstime) }

                let prehash = _previousblockhash templateData1

                lasthash <- getPoolStatsField poolStats pLastHash
           
                updatePoolStats  poolStats pLastHash $ \_->prehash

                when (prehash /= lasthash) $
                     updatePoolStats  poolStats pBlockChangeId $ \_->jobId'
                
                    --累加任务号    
                modifyIORef' jobIdRef (+1) 
                jobId1 <- readIORef jobIdRef

                updatePoolStats  poolStats pWorkbaseId $ \_->jobId1

                let (tp,jobData) = encodeNotifyData jobStr templateData1
                writeIORef curJob jobData

                --加入任务表 -- atomicModifyIORef'
                modifyIORef' jobPool $ Map.insert jobStr tp

                --重新读取过虑后的所有job
                jobMap <- readIORef jobPool
                logInfo $ "jobStr :" <>  displayShow jobStr <> " newdif : " <> displayShow netdiff <> " secstime: " <> displayShow secstime <> " size : " <> (displayShow $ Map.size jobMap)                    

                when (prehash /= lasthash)  $ do
                    updateJob jobData

            _ -> return ()   

addSubmit :: (IORef PoolStats) -> AccountPool -> MinerPool -> String -> String -> Double -> Bool -> Bool -> RIO App ()
addSubmit poolStats accountPool minerPool sessionid username diff1 valid submit = do
    if valid == True 
    then do
        updatePoolStats poolStats pUnaccountedShares (\c -> c + 1)
        updatePoolStats poolStats pUnaccountedDiffShares (\c -> c + 1)
        updateMinerClient minerPool sessionid shares (\c -> c + 1)
        updateMinerAccount accountPool username aShares (\c -> c + 1)
    else do
        updatePoolStats poolStats pUnaccountedRejects (\c -> c + 1)
        updateMinerClient minerPool sessionid sharerej (\c -> c + 1)
        updateMinerAccount accountPool username aSharerej (\c -> c + 1)
        
     
     


calBlockHash :: Text -> [Text] -> TemplateData -> [Word8]
calBlockHash  nonceSuf tx templateData =  ver <> preHash <> mklRoot <> rootHash <> time <> nbits <> nonceValue <> solution
    where ver = word32ToLeWord8 $ _version templateData      
          preHash = hex2Bin . unpack . reverseHash' . _previousblockhash $ templateData
          mklRoot = hex2Bin . unpack . liftMaybe . _merkleroot $ templateData
          rootHash = hex2Bin . unpack . reverseHash' . _finalsaplingroothash $ templateData --倒序
          time = hex2Bin . unpack  $ tx !! 2 --倒序
          nbits = hex2Bin . unpack . reverseHash' .  _bits  $ templateData --倒序
          nonceValue = hex2Bin . unpack $ (nonceSuf) <> (tx !! 3)
          solution = hex2Bin . unpack $ tx !! 4
        
-- 组装notify的数据
encodeNotifyData :: Text -> TemplateData -> (TemplateData,Value)
encodeNotifyData job templateData = (newData,v)
    where ver = pack $ bin2Hex $ word32ToLeWord8 $ _version templateData
          preHash = reverseHash' $ _previousblockhash templateData
          cbHash = T.encodeUtf8 . _hash . _coinbasetxn $ templateData
          transHash = map (T.encodeUtf8 . _hash) (_transactions templateData)
          -- 计算merkleroot
          merklert = T.decodeUtf8 $ merkleRoot (cbHash:transHash)
          rootHash = reverseHash' $ _finalsaplingroothash templateData
          time = time2Text $ _curtime templateData
          nbits = reverseHash' $ _bits templateData
          --将merkleroot值设置到templateData中
          newData = set merkleroot (Just merklert) templateData
          v = Array (V.fromList [String job,String ver,String preHash, String merklert, String rootHash, String time,String nbits,Bool True])


getPoolStatsField :: IORef PoolStats -> Lens' PoolStats a -> RIO App a
getPoolStatsField poolStats l = do
    pStats <- readIORef poolStats
    return $ view l pStats

updatePoolStats :: IORef PoolStats -> Lens' PoolStats a -> (a -> a) -> RIO App ()
updatePoolStats poolStats l f = do
    pStats <- readIORef poolStats
    let aa = view l pStats
    let n = f aa
    let pStats1 = set l n pStats
    writeIORef poolStats pStats1


updateMinerClient :: MinerPool -> MinerID -> Lens' MinerClient a -> (a -> a) -> RIO App () 
updateMinerClient minerPool sessionid l f = do
                                            minerClients <- readIORef minerPool

                                            let mbMinerClient =  Map.lookup sessionid minerClients
                                            case mbMinerClient of
                                                Nothing ->  logInfo $ "noexist sessionid:" <> displayShow sessionid
                                                Just minerClient -> do
                                                                --logInfo $  displayShow $ _sessionId minerClient
                                                                      let aa = view l minerClient
                                                                      let n = f aa
                                                                      let minerClient' = set l n minerClient
                                                                      modifyIORef' minerPool $ Map.insert sessionid minerClient'

getMinerClientField :: MinerPool -> MinerID -> Lens' MinerClient a -> a -> RIO App a 
getMinerClientField minerPool sessionid l dv = do
                                            minerClients <- readIORef minerPool

                                            let mbMinerClient =  Map.lookup sessionid minerClients
                                            case mbMinerClient of
                                                Nothing -> return dv
                                                Just minerClient -> return $ view l minerClient
                                                                                                                                        

updateMinerAccount :: AccountPool -> AccountID -> Lens' MinerAccount a -> (a -> a) -> RIO App () 
updateMinerAccount accountPool username l f = do
                                            minerAccounts <- readIORef accountPool

                                            let mbMinerAccount =  Map.lookup username minerAccounts
                                            case mbMinerAccount of
                                                Nothing ->   modifyIORef' accountPool $ Map.insert username $ MinerAccount username 1 def def def
                                                                                                                            def def def def def
                                                                                                                            def def def def def 
                                                                                                                            True 
                                                Just minerAccount -> do
                                                                --logInfo $  displayShow $ _sessionId minerClient
                                                                      let aa = view l minerAccount
                                                                      let n = f aa
                                                                      let minerAccount' = set l n minerAccount
                                                                      modifyIORef' accountPool $ Map.insert username minerAccount'
getMinerAccountField :: AccountPool -> AccountID -> Lens' MinerAccount a -> a -> RIO App a
getMinerAccountField accountPool username l dv = do
                                            minerAccounts <- readIORef accountPool
                                            let mbMinerAccount =  Map.lookup username minerAccounts
                                            case mbMinerAccount of
                                                Nothing -> return dv
                                                Just minerAccount -> return $ view l minerAccount
