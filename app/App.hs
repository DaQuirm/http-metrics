module App where

import Control.Monad.Reader (ReaderT, ask)
import Control.Concurrent.MVar (MVar)

import Servant (Handler, ServerT)

type ServiceState = ()
type AppT m = ReaderT (MVar ServiceState) m

type API api = ServerT api (AppT Handler)