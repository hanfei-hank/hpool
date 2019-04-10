{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( mboxSend
  , connReader
  , tcpServer
  , lensyToJSON
  , lensyParseJSON
  , randomText
  , randomText'
  , randomNonce
  , list2TupleList
  , word32ToHexStr
  , word64ToHexStr
  , word8toHexStr
  , word32ToLeWord8
  , word32ToBeWord8
  , hexStrtoWord8
  , bin2Hex
  , hex2Bin
  , le64toh
  , diffFromTarget
  , targetFromDiff
  , le256todouble
  , word64ToLeWord8
  , getMillisTimeStamp
  , getSecsTimeStamp
  , randomWord32
  , randomWord64
  , reverseHash
  , reverseHash'
  , reverseHash1
  , merkleRoot
  , time2Text
  , calculateTrans
  , int2Hex
  , genHash
  , sha256
  , liftMaybe
  ) where

import           RIO hiding (reverse)
import           Data.Aeson.Types
import           GHC.Generics(Rep)
import           Data.Char(toLower)
import qualified Data.Attoparsec.ByteString as A
import           Network.Socket (Socket)
import           System.Random (randomIO)
import qualified Data.ByteString as B
import           Numeric(showHex)
import           System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams
import           Data.Connection
import qualified System.IO.Streams.TCP as TCP
import           Text.StringRandom
import           Data.ByteString.Base16
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Bits
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteArray as B (convert)
import           Crypto.Hash (Digest, hash,SHA256)
import           Data.List ((!!),length,splitAt)
import           Data.Time.Clock.POSIX (getPOSIXTime)
 




mboxSend :: (MonadIO m) => OutputStream a -> a -> m ()
mboxSend outs a = liftIO $ Streams.write (Just a) outs

-- 解析request
connReader :: A.Parser a -> OutputStream a -> Connection e -> RIO env ()
connReader parser outs conn = liftIO $ forever $ streamingParser ""
    where
        ins = source conn
        recv = do
            mbs <- Streams.read ins
            case mbs of
                Just bs -> return bs
                Nothing -> throwString "peer closed connection"

        streamingParser leftovers = do
            result <- A.parseWith recv parser leftovers
            case result of
                A.Done rest result -> do
                    Streams.write (Just result) outs
                    streamingParser rest
                A.Fail _ _ msg -> throwString msg
 
reportException action = withException action $ \e -> logError $ display @SomeException e

-- 监听socket，为每个进来的连接启动一个处理线程
tcpServer :: (HasLogFunc env) => Socket -> (TCP.TCPConnection -> RIO env ()) -> RIO env ()
tcpServer sock action = forever $ do
    conn <- liftIO $ TCP.accept sock
    async $ do
        reportException (action conn) `finally` do
            logInfo $ "closing conn"
            liftIO $ close conn


lensyConstructorToNiceJson :: Int -> String -> String
lensyConstructorToNiceJson n fieldName = firstToLower $ drop n fieldName
  where
    firstToLower (c:cs) = toLower c : cs
    firstToLower _ = error "You've managed to screw up the drop number or the field name"

lensyOptions :: Int -> Options
lensyOptions n = defaultOptions { fieldLabelModifier = lensyConstructorToNiceJson n }

lensyToJSON
  :: (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
lensyToJSON n = genericToJSON (lensyOptions n)

lensyParseJSON
  :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
lensyParseJSON n = genericParseJSON (lensyOptions n)

--返回一个8位数字与字母的随机数 如:gbiaf504
randomText :: IO Text
randomText = stringRandomIO "^[a-zA-Z\\d]{4}"

randomText' :: Int -> IO Text
randomText' lg = stringRandomIO $ "^[a-z\\d]{" <> (tshow lg) <>"}"

-- 返回一个随机的nonce和sessionId
randomNonce :: IO (Text, Text)
randomNonce = do
    str <- randomText
    let revStr = T.reverse str
    return (T.decodeUtf8 $ encode $ T.encodeUtf8 str, T.decodeUtf8 $ encode $ T.encodeUtf8 revStr)
-- 将hash字符反转
reverseHash :: String -> String
reverseHash str | length str >2 = reverseHash l ++ f
              | otherwise = f ++ []
              where (f,l) = splitAt 2 str
-- 包装一个Text版本
reverseHash' :: Text -> Text
reverseHash' = T.pack . reverseHash . T.unpack





word8toHexStr :: Word8 -> String
word8toHexStr w = do
                    let hex = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
                    let p1 = fromIntegral (w `shiftR` 4) :: Int
                    let p2 = fromIntegral (w .&. 0x0f) :: Int
                    (hex !! p1) : (hex !! p2) : []                                      

                  
hexStrtoWord8 :: String -> Word8
hexStrtoWord8 str = do
                        let p = C.pack str
                        let w = B.unpack p
                        let hex2bin_tbl= [  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                             0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
                                            -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
                                            -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 ]

                        let p1 = fromIntegral (w !! 0) :: Int
                        let p2 = fromIntegral (w !! 1) :: Int
                        let b1 = hex2bin_tbl !! p1
                        let b2 = hex2bin_tbl !! p2
                        (b1 `shiftL` 4) .|. b2

list2TupleList:: [a] -> [[a]]
list2TupleList list = do
                        let  len = length list  
                        case len of
                                0 -> []
                                _ -> do
                                        let t = splitAt 2 list
                                        (fst t) : (list2TupleList $ snd t)
                                                
--16进制字符串转字节数组                           
hex2Bin :: String -> [Word8]
hex2Bin str = do
                let hexTuple = list2TupleList str
                map hexStrtoWord8 hexTuple
 
--字节数组转16进制字符串                                          
bin2Hex :: [Word8] -> String
bin2Hex list = concatMap word8toHexStr list


le64toh ::[Word8] -> Word64
le64toh  w = (fromIntegral (w !! 0) :: Word64) + (fromIntegral (w !! 1) `shiftL` 8 :: Word64)
            + (fromIntegral (w !! 2) `shiftL` 16  :: Word64) + (fromIntegral (w !! 3) `shiftL` 24 :: Word64)
            + (fromIntegral (w !! 4) `shiftL` 32 :: Word64) + (fromIntegral (w !! 5) `shiftL` 40  :: Word64)
            + (fromIntegral (w !! 6) `shiftL` 48  :: Word64)+ (fromIntegral (w !! 7) `shiftL` 56 :: Word64)

le32toh ::[Word8] -> Word32
le32toh  w = (fromIntegral (w !! 0) :: Word32) + (fromIntegral (w !! 1) `shiftL` 8 :: Word32)
            + (fromIntegral (w !! 2) `shiftL` 16  :: Word32) + (fromIntegral (w !! 3) `shiftL` 24 :: Word32)

bits192 = 6277101735386680763835789423207666416102355444464034512896.0 :: Double
bits128 = 340282366920938463463374607431768211456.0 :: Double
bits64 = 18446744073709551616.0 :: Double
truediffone = 3618502788666131106986593281521497120414687020801267626233049500247285301247.0 :: Double
            
le256todouble :: [Word8] -> Double
le256todouble w8list = do      
                        let l24 = drop 24 w8list
                        let l16 = take 8 $ drop 16 w8list
                        let l8 = take 8 $ drop 8 w8list
                        let l0 = take 8 w8list              
                        let dblel24 = fromIntegral (le64toh l24) :: Double
                        let dblel16 = fromIntegral (le64toh l16) :: Double
                        let dblel8 = fromIntegral (le64toh l8) :: Double
                        let dblel0 = fromIntegral (le64toh l0) :: Double     
                        dblel24 * bits192 + dblel16 * bits128 + dblel8 * bits64 + dblel0

diffFromTarget :: String -> Double
diffFromTarget str = do 
                         let w8list = hex2Bin str
                         let dcut64 = le256todouble w8list
                         if  dcut64 == 0 
                         then
                             truediffone/1
                         else             
                             truediffone/dcut64     

targetFromDiff :: Double ->  String
targetFromDiff diff = do
                        case diff of 
                            0 -> "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" 
                            _ -> do   
                                    let d64 = truediffone/diff
                                    let dcut64 = d64/bits192
                                    let h64 = round dcut64 :: Word64
                                    let l24 = word64ToLeWord8 h64
                                    
                                    let d64' = d64 - dcut64
                                    let dcut64' = d64'/bits128
                                    let h64' = round dcut64' :: Word64
                                    let l16 = word64ToLeWord8 h64'
                                    
                                    let d64'' = d64' - dcut64'
                                    let dcut64'' = d64''/bits64
                                    let h64'' = round dcut64'' :: Word64
                                    let l8 = word64ToLeWord8 h64''
                                
                                    let d64''' = d64'' - dcut64''
                                    let dcut64''' = d64'''
                                    let h64''' = round dcut64''' :: Word64
                                    let l0 = word64ToLeWord8 h64'''
                                    bin2Hex $ l0 <> l8 <> l16 <> l24                                           
                                
word64ToLeWord8 :: Word64 -> [Word8]
word64ToLeWord8 w = do
                    let p1 = fromIntegral (w `shiftR` 56) .&. 0xff ::Word8
                    let p2 = fromIntegral (w `shiftR` 48) .&. 0xff ::Word8
                    let p3 = fromIntegral (w `shiftR` 40) .&. 0xff  ::Word8
                    let p4 = fromIntegral (w `shiftR` 32) .&. 0xff  ::Word8
                    let p5 = fromIntegral (w `shiftR` 24) .&. 0xff ::Word8
                    let p6 = fromIntegral (w `shiftR` 16) .&. 0xff ::Word8
                    let p7 = fromIntegral (w `shiftR` 8) .&. 0xff  ::Word8
                    let p8 = fromIntegral (w .&. 0xff) ::Word8
                    [p8,p7,p6,p5,p4,p3,p2,p1]

decayTime :: Double->Double->Double->Double
decayTime fadd fsecs interval = do
                                  let dexp' = fsecs/interval
                                  let dexp = min dexp' 36
                                  let expv = exp dexp
                                  let fprop = 1.0 - 1 / expv
                                  let ftotal = 1.0 + fprop

                                  let f' = fadd / fsecs * fprop
                                  let f = f' / ftotal

                                  if f >= 2e-16 
                                    then f
                                    else 0.0 ::Double  
                                                          

--４字节小端端格式
word32ToLeWord8 :: Word32 -> [Word8]
word32ToLeWord8 w = do 
                    let p1 = fromIntegral (w `shiftR` 24) .&. 0xff ::Word8
                    let p2 = fromIntegral (w `shiftR` 16) .&. 0xff ::Word8
                    let p3 = fromIntegral (w `shiftR` 8) .&. 0xff  ::Word8
                    let p4 = fromIntegral (w .&. 0xff) ::Word8
                    [p4,p3,p2,p1]
--４字节大端端格式
word32ToBeWord8 :: Word32 -> [Word8]
word32ToBeWord8 w = do 
                    let p1 = fromIntegral (w `shiftR` 24) .&. 0xff ::Word8
                    let p2 = fromIntegral (w `shiftR` 16) .&. 0xff ::Word8
                    let p3 = fromIntegral (w `shiftR` 8) .&. 0xff  ::Word8
                    let p4 = fromIntegral (w .&. 0xff) ::Word8
                    [p1,p2,p3,p4]

--4字节转16进制字符串
word32ToHexStr :: Word32 -> String
word32ToHexStr w =  bin2Hex $  word32ToBeWord8 w

--8字节转16进制字符串
word64ToHexStr :: Word64 -> String
word64ToHexStr w =  bin2Hex $  word64ToLeWord8 w
                       
randomWord32 :: IO Word32
randomWord32 = randomIO

randomWord64 :: IO Word64
randomWord64 = randomIO


-- 包装一个Text版本
reverseHash1 :: ByteString -> ByteString
reverseHash1 = C.pack . reverseHash . C.unpack

-- 求交易的markleRoot hash
merkleRoot :: [ByteString] -> ByteString
merkleRoot ls =  B.convertToBase B.Base16 $ merkle nls
    where nls = map decodeByteString ls 

decodeByteString:: ByteString -> ByteString
decodeByteString b = v
    where b1 = reverseHash1 b
          (v,_) = decode $ b1

genHash :: ByteString -> ByteString
genHash x = B.convert (hash x :: Digest SHA256)

sha256 :: ByteString -> ByteString
sha256 x = B.convert (hash (hash x :: Digest SHA256) :: Digest SHA256)

dhash256 :: ByteString -> ByteString -> ByteString
dhash256 a b = sha256 ( a <> b)

merkle :: [ByteString] -> ByteString
merkle ls | length ls == 0 = ""
          | length ls == 1 = ls !! 0 --如果长度为1返回第一个元素
          | otherwise = merkle $ merkleArray ls

merkleArray :: [ByteString] -> [ByteString]
merkleArray ls | length ls ==0 = []
               | length ls ==1 = ls
               | otherwise = if (length r ==1) then sHash:(merkleArray ((r !! 0):r)) else sHash:(merkleArray r)
               where (l,r) = splitAt 2 ls
                     sHash = dhash256 (l !! 0) (l !! 1)

--补0
time2Text :: Int -> Text
time2Text i = T.pack $ reverseHash tx
    where te = showHex i ""
          ln = length te
          tx = if(ln `rem` 2 ==0) then te else ('0':te)

--第二个参数为长度
int2Hex :: Int -> Int -> Text
int2Hex num lh | ln > lh = hx
               | otherwise = (T.take (lh-ln) zero) <> hx
                where hx = T.pack $ showHex num ""
                      ln = T.length hx
                      zero = "000000000000000000"

--计算交易拼接值
calculateTrans :: Int -> Text
calculateTrans num | num < 253 = int2Hex num 2
                   | num <= 65535 = "fd" <> (int2Hex num 4)
                   | otherwise = "fe" <> (int2Hex num 8)
                   
getMillisTimeStamp :: IO Integer
getMillisTimeStamp = (round . (* 1000)) <$> getPOSIXTime          

getSecsTimeStamp :: IO Integer
getSecsTimeStamp = round  <$> getPOSIXTime


liftMaybe :: Maybe Text -> Text
liftMaybe Nothing = ""
liftMaybe (Just a) = a


