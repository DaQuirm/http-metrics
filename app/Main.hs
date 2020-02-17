{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (ToJSON, FromJSON)
import Servant
import Servant.Server (serve)
import Network.WebSockets (Connection, withPingThread, sendTextData)
import Servant.API.WebSocket (WebSocket)

-- import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import App

appTToHandler :: MVar ServiceState -> AppT Handler a -> Handler a
appTToHandler stateVar value = runReaderT value stateVar

data Metric = Metric
    { metricName :: Text
    , metricValue :: Int
    } deriving Generic

instance ToJSON Metric

type MetricAPI
  = "metrics" :>
    (    Get '[JSON] [Metric]
    )

metricAPI :: API MetricAPI
metricAPI
  =    getMetric
  where
    getMetric :: AppT Handler [Metric]
    getMetric =
      pure [Metric "test" 37]

type WebSocketApi = "stream" :> WebSocket

wsServer :: API WebSocketApi
wsServer = streamData
 where
  streamData :: MonadIO m => Connection -> m ()
  streamData connection = do
    liftIO $ withPingThread connection 10 (pure ()) (pure ())
    liftIO . forM_ [1..] $ \i -> do
       sendTextData connection $ pack $ show i
       threadDelay 1000000

type ServiceAPI
  = MetricAPI
  :<|> WebSocketApi

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

metricMiddleware :: Middleware
metricMiddleware app = \request responseFunc ->
  app request

main :: IO ()
main = do
    -- connectionPool <- createPool (pgOpen connectionSettings) seldaClose 1 30 16
    stateVar <- newMVar ()
    let server = metricAPI :<|> wsServer
        app = serve
            serviceAPI
            (hoistServer serviceAPI (appTToHandler stateVar) server)
    run 3000 app
