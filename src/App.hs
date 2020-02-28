{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module App where

import GHC.Generics (Generic)
import Control.Lens (makeLenses)
import Control.Monad.Reader (ReaderT)
import Control.Concurrent.MVar (MVar)
import Data.Aeson (ToJSON)
import Servant (Handler, ServerT)

data ServiceState = ServiceState
  { _requestsTotal     :: !Int
  , _payloadBytesTotal :: !Int
  , _exceptionsTotal   :: !Int
  } deriving Generic

makeLenses ''ServiceState

instance ToJSON ServiceState

emptyState :: ServiceState
emptyState = ServiceState { _requestsTotal = 0, _payloadBytesTotal = 0, _exceptionsTotal = 0 }

type AppT m = ReaderT (MVar ServiceState) m

type API api = ServerT api (AppT Handler)