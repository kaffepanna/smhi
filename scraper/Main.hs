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
import System.Environment

import Types
import JSON
import Persistence
import Network

import Data.Text
import Data.Maybe

data Env m = Env { _persistence :: Persistence
                 , _network :: Network
                 , _logAction :: !(LogAction m Message) }

newtype App e = App { unapp :: ReaderT (Env App) IO e }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (Env App))

instance HasPersistence (Env m) where
    getPersistence = _persistence

instance HasNetwork (Env m) where
    getNetwork = _network

instance HasLog (Env m) Message m where
    getLogAction = _logAction
    setLogAction newLogAction env = env { _logAction = newLogAction }

withApp :: Env App -> App () -> IO ()
withApp env f = runReaderT (unapp f) env

-- environment variable with default value in io monad
fromEnv :: String -> String -> IO String
fromEnv d = fmap (fromMaybe d) . lookupEnv

main :: IO ()
main = do
    database <- fromEnv "data.db" "DATABASE"

    env <- Env <$> initPersistence database
               <*> initNetwork
               <*> pure richMessageAction

    withApp env $ do
        prepare Observation
        prepare Forecast

        fetch Forecast    >>= upsert Forecast
        fetch Observation >>= upsert Observation
