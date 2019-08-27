{-# LANGUAGE MultiParamTypeClasses
  , OverloadedStrings
  , InstanceSigs
  , FlexibleInstances
  , FlexibleContexts
  , ConstraintKinds
  , AllowAmbiguousTypes #-}

module Network where

import Prelude hiding (log)
import Data.Text
import Network.HTTP.Client
import Control.Monad.Reader
import Control.Monad.Identity
import Colog

import Types
import JSON

forecastUrl = "http://www.smhi.se/wpt-a/backend_tendayforecast/forecast/fetcher/2667109/10dFormat"
observationsUrl = "http://www.smhi.se/wpt-a/backend_tendayforecast/analys/fetcher/2667109/10dFormat"

newtype Network = Network { _manager :: Manager }

type WithNetwork env m =  (MonadReader env m, MonadIO m, HasNetwork env)

class HasNetwork env where
    getNetwork :: env -> Network


instance HasNetwork Network where
    getNetwork = id

initNetwork :: (MonadIO m) => m Network
initNetwork = do m <- liftIO $ newManager defaultManagerSettings
                 return $ Network m

fetchUrl :: (WithLog env Message m, WithNetwork env m) => Request -> m [Hour]
fetchUrl url = do
    Network manager <- asks getNetwork
    resp <- liftIO $ httpLbs url manager
    let body = responseBody resp
    case parseHoursE body of
      Right d -> return d
      Left e -> do
          log W $ "Could not parse response " <> pack e
          return []
      

fetch :: (WithLog env Message m, WithNetwork env m) => RecordType -> m [Hour]
fetch Forecasts = do
    hours <- fetchUrl forecastUrl
    let lh = Prelude.length hours
    log I $ "Fetched " <> pack (show lh) <> " forecasts"
    return hours

fetch Observations = do
    hours <- fetchUrl observationsUrl
    let lh = Prelude.length hours
    log I $ "Fetched " <> pack (show lh) <> " observations"
    return hours

