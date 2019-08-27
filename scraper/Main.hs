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

import Env
import Types
import JSON
import Persistence
import Network

import Data.Text
import qualified Data.ByteString.Char8 as Bs
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

initPersistence :: String -> IO Persistence 
initPersistence cs = do
    conn <- connectPostgreSQL (Bs.pack cs)
    let runBeam = runBeamPostgres conn
    return $ Persistence runBeam

main :: IO ()
main = do
    database <- fromEnv "host=127.0.0.1 port=11632 dbname=smhi password=******* user=admin" "DATABASE"

    env <- Env <$> initPersistence database
               <*> initNetwork
               <*> pure richMessageAction

    withApp env $ do
        fetch Forecasts >>= upsertForecasts
        fetch Observations >>= upsertObservations
