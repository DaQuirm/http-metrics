{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Control.Lens (Lens')
import Control.Lens.Operators ((^.), (+~))
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Network.Wai (Middleware, requestMethod, requestBodyLength, RequestBodyLength(..))
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setOnException, setPort)
import Servant
import Servant.Server (serve)
import Network.WebSockets (Connection, withPingThread, sendTextData)
import Servant.API.WebSocket (WebSocket)
import Data.Aeson (ToJSON, encode)

import App

appTToHandler :: MVar ServiceState -> AppT Handler a -> Handler a
appTToHandler stateVar value = runReaderT value stateVar

data Metric a = Metric
  { name :: Text
  , value :: a
  } deriving Generic

instance ToJSON a => ToJSON (Metric a)

type MetricAPI
    =    "metrics" :> Get '[JSON] ServiceState
    :<|> "metrics" :> "requestsTotal"     :> Get '[JSON] (Metric Int)
    :<|> "metrics" :> "exceptionsTotal"   :> Get '[JSON] (Metric Int)
    :<|> "metrics" :> "payloadBytesTotal" :> Get '[JSON] (Metric Int)

getMetric :: Text -> Lens' ServiceState a -> AppT Handler (Metric a)
getMetric name lens = do
  stateVar <- ask
  state <- liftIO $ readMVar stateVar
  pure $ Metric name $ state ^. lens

metricAPI :: API MetricAPI
metricAPI
  =    getAllMetrics
  :<|> getMetric "requestsTotal"     requestsTotal
  :<|> getMetric "exceptionsTotal"   exceptionsTotal
  :<|> getMetric "payloadBytesTotal" payloadBytesTotal
    where
      getAllMetrics :: AppT Handler ServiceState
      getAllMetrics = do
        stateVar <- ask
        liftIO $ readMVar stateVar

type CrashAPI = "crash" :> Post '[JSON] NoContent

crashAPI :: API CrashAPI
crashAPI
  = crashService
    where
      crashService :: AppT Handler NoContent
      crashService = do
        error "Crash!"
        pure NoContent

type WebSocketApi = "stream" :> WebSocket

wsServer :: API WebSocketApi
wsServer = streamData
 where
  streamData connection = do
    stateVar <- ask
    liftIO $ withPingThread connection 10 (pure ()) (pure ())
    liftIO $ forever $ do
      state <- readMVar stateVar
      sendTextData connection $ encode state
      threadDelay 1000000

type ServiceAPI
  = MetricAPI
  :<|> WebSocketApi
  :<|> CrashAPI

serviceAPI :: Proxy ServiceAPI
serviceAPI = Proxy

metricMiddleware :: MVar ServiceState -> Middleware
metricMiddleware stateVar app request responseFunc = do
  let payloadSize = case requestBodyLength request of
                      ChunkedBody -> 0
                      KnownLength l -> fromIntegral l

  modifyMVar_ stateVar $ pure . (requestsTotal +~ 1)
  modifyMVar_ stateVar $ pure . (payloadBytesTotal +~ payloadSize)
  app request responseFunc

exceptionHandler stateVar _ exception = do
  print exception
  modifyMVar_ stateVar $ pure . (exceptionsTotal +~ 1)

main :: IO ()
main = do
    stateVar <- newMVar emptyState
    let server = metricAPI :<|> wsServer :<|> crashAPI
        app = serve
            serviceAPI
            (hoistServer serviceAPI (appTToHandler stateVar) server)
    runSettings
      ( setOnException (exceptionHandler stateVar)
      $ setPort 3000
        defaultSettings
      )
      (metricMiddleware stateVar app)
