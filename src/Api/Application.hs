{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Application where
import Prelude hiding (log)

import Colog
import Control.Monad.Trans
import Control.Monad.Reader
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Persistence
import JSON
import Types
import Api.Mae

data ApplicationRoutes route = ApplicationRoutes { _mae :: route :- "mae" :> ToServantApi MAERoutes
                                                 , _static :: route :- Raw } deriving (Generic)


appServer :: (WithLog e Message m, WithBeam e m, MonadReader e m) => ApplicationRoutes (AsServerT m)
appServer = ApplicationRoutes { _mae = toServant maeServer
                              , _static = serveDirectoryFileServer "static" }

