{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Main where
import Prelude hiding (log)
import Colog
import Colog.Actions as CA

import Control.Monad.Reader
import Control.Monad.Trans

import Data.Pool
import Data.Proxy
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)
import Database.Beam.Postgres
import Database.PostgreSQL.Simple

import System.IO

import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Network.Wai.Handler.Warp 

import Lib
import Env
import Types
import Persistence
import JSON
import Api.Application

data Env m = Env { _persistence :: Persistence
                 , _logAction :: !(LogAction m Message) }

newtype App e = App { unapp :: ReaderT (Env App) Handler e }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (Env App))

instance HasBeam (Env m) where
    getPersistence = _persistence

instance HasLog (Env m) Message m where
    getLogAction = _logAction
    setLogAction newLogAction env = env { _logAction = newLogAction }


runApp :: Env App -> App a -> Handler a
runApp env f = runReaderT (unapp f) env

application :: Env App -> Application
application env = genericServeT (runApp env) appServer


logRequests logAction req c i = usingLoggerT logAction (logInfo $ "[request] " <> T.pack (show req))

settings la = setPort 3000
         $ setLogger (logRequests la)
         $ setHost "0.0.0.0"
         defaultSettings

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    dbConnString <-  "host="     <+> getEnv "DB_HOST"
                 <> " port="     <+> getEnv "DB_PORT"
                 <> " user="     <+> getEnv "DB_USER"
                 <> " password=" <+> getEnv "DB_PASSWORD"


    let logAction = richMessageAction

    env <- Env <$> initPersistence richMessageAction (fromJust dbConnString)
               <*> pure richMessageAction

    putStrLn "Starting server on 3000"
    runSettings (settings logAction) (application env)
