{-# LANGUAGE TemplateHaskell #-}

module App where

import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT)
import Control.Concurrent.MVar (MVar)

import Servant (Handler, ServerT)

data ServiceState = ServiceState { _requestsTotal :: !Int, _bytesTotal :: !Int }

makeLenses ''ServiceState

emptyState :: ServiceState
emptyState = ServiceState { _requestsTotal = 0, _bytesTotal = 0 }

type AppT m = ReaderT (MVar ServiceState) m

type API api = ServerT api (AppT Handler)