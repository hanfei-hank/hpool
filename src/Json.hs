module Json where

import RIO
import RIO.Partial (fromJust)
import Data.Aeson
import Data.Aeson.Types
--
-- Requests
--
type Method = Text
type Id     = Int
type ErrorObj = Value

data Request = Request { getReqMethod :: !Method
                       , getReqParams :: !Value
                       , getReqId     :: !Id
                       }
             | Notif   { getReqMethod :: !Method
                       , getReqParams :: !Value
                       }
             deriving (Eq, Show)

instance FromJSON Request where
    parseJSON = withObject "request" $ \o -> do
        (n, m, p) <- parseIdMethParams o
        case n of 
            Nothing -> return $ Notif   m p
            Just i  -> return $ Request m p i
        
        where
            parseIdMethParams o = do
                i <- o .:? "id"
                m <- o .: "method"
                p <- o .:? "params" .!= Null
                return (i, m, p)

instance ToJSON Request where
    toJSON (Request m p i) = object $ case p of
        Null -> ["method" .= m, "id" .= i, "params" .= emptyArray]
        _    -> ["method" .= m, "id" .= i, "params" .= p]
    toJSON (Notif m p) = object $ case p of
        Null -> ["method" .= m, "params" .= emptyArray, "id" .= Null]
        _    -> ["method" .= m, "params" .= p, "id" .= Null]


--
-- Responses
--

data Response = Response      { getResult :: !Value
                              , getResId  :: !Id
                              }
              | ResponseError { getError  :: !ErrorObj
                              , getResId  :: !Id
                              }
              deriving (Eq, Show)

instance FromJSON Response where
    parseJSON = withObject "response" $ \o -> do
        (d, s) <- parseIdResultError o
        case s of
            Right r -> do
                guard $ isJust d
                return $ Response r (fromJust d)
            Left e -> do
                guard $ isJust d
                return $ ResponseError e (fromJust d)

        where
            parseIdResultError o = do
                i <- o .:? "id"
                r <- o .:? "result" .!= Null
                p <- if r == Null then Left <$> o .: "error" else return $ Right r
                return (i, p)                   

instance ToJSON Response where
    toJSON (Response r i) = object
        ["id" .= i, "result" .= r, "error" .= Null]
    toJSON (ResponseError e i) = object
        ["id" .= i, "error" .= e, "result" .= Null]

okResponse :: Id -> Response
okResponse = Response (toJSON True)

errorResponse :: Id -> Text -> Response
errorResponse id error = ResponseError (String error) id

resultResponse :: ToJSON a => Id -> a -> Response
resultResponse id v = Response (toJSON v) id