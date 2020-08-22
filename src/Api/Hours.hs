{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Hours where

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

newtype HoursRoutes route = HoursRoutes { _forecasts :: route :- "forecasts" :> Get '[JSON] [Forecast]
                                    } deriving (Generic)

hoursServer :: (WithLog e Message m, WithBeam e m, MonadReader e m) => HoursRoutes (AsServerT m)
hoursServer = HoursRoutes { _forecasts = log I "get forecasts" >> getHours Forecasts }
