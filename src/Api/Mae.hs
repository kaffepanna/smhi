{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.Mae where
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

newtype MAERoutes route = MAERoutes { _get :: route :- Get '[JSON] [MAE]
                                    } deriving (Generic)

maeServer :: (WithLog e Message m, WithBeam e m, MonadReader e m) => MAERoutes (AsServerT m)
maeServer = MAERoutes { _get = log I "get mae" >> getMAE }
