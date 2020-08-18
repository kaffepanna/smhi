{-# LANGUAGE OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , InstanceSigs #-}
module Main where
import Prelude hiding (log)
import Colog
import Colog.Actions as CA
import Control.Applicative ((<|>))
import Colog.Message
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader

import Lib
import Env
import Types
import JSON
import Persistence
import Network

import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe

import Database.Beam.Postgres
import Database.PostgreSQL.Simple

data Env m = Env { _persistence :: Persistence
                 , _network :: Network
                 , _logAction :: !(LogAction m Message) }

newtype App e = App { unapp :: ReaderT (Env App) IO e }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (Env App))

instance HasBeam (Env m) where
    getPersistence = _persistence

instance HasNetwork (Env m) where
    getNetwork = _network

instance HasLog (Env m) Message m where
    getLogAction = _logAction
    setLogAction newLogAction env = env { _logAction = newLogAction }

withApp :: Env App -> App () -> IO ()
withApp env f = runReaderT (unapp f) env

-- initPersistence :: Text -> IO Persistence 
-- initPersistence cs = do
--     conn <- connectPostgreSQL (encodeUtf8 cs)
--     let runBeam = runBeamPostgres conn
--     return $ Persistence runBeam

main :: IO ()
main = do
    dbConnString <-  "host="     <+> getEnv "DB_HOST"
                 <> " port="     <+> getEnv "DB_PORT"
                 <> " user="     <+> getEnv "DB_USER"
                 <> " password=" <+> getEnv "DB_PASSWORD"

    env <- Env <$> initPersistence richMessageAction (fromJust dbConnString)
               <*> initNetwork
               <*> pure richMessageAction

    withApp env $ do
        fetch Forecasts >>= upsertForecasts
        fetch Observations >>= upsertObservations
