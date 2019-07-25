{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds #-}
module Persistence where

import Prelude hiding (log)

import Control.Monad.Reader
import Control.Monad (forM_)
import Database.SQLite.Simple
import Colog

import Data.Text (pack)
import Persistence.Queries
import Types
import Network

newtype Persistence = Persistence { _connection :: Connection }

type WithPersistence env m = (MonadReader env m, MonadIO m, HasPersistence env)

class HasPersistence env where
    getPersistence :: env -> Persistence

instance HasPersistence Persistence where
    getPersistence = id

instance ToRow Hour where
    toRow (Hour a as ae t p) = toRow (a, as, ae, t, p)

instance FromRow Hour where
    fromRow = Hour <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance FromRow MAE where
    fromRow = MAE <$> field
                  <*> field
                  <*> field
                  <*> field

instance FromRow HourOffset where
    fromRow = HourOffset <$> field
                         <*> field
                         <*> field

getHourOffsets :: (WithPersistence env m) => m [HourOffset]
getHourOffsets = do
    Persistence conn <- asks getPersistence
    liftIO $ query_ conn getHoursQ

getMAE :: (WithLog env Message m, WithPersistence env m) => m [MAE]
getMAE = do
    Persistence conn <- asks getPersistence
    liftIO $ query_ conn getMAEQ

insert :: (WithLog env Message m, WithPersistence env m) => Query -> [Hour] -> m ()
insert query hours = do
    Persistence conn <- asks getPersistence
    cs <- forM hours $ \h -> do
            liftIO $ execute conn query h
            liftIO $ changes conn
    let c = sum cs
    log I $ "inserted " <>  pack (show c) <> " records"

upsert :: (WithLog env Message m, WithPersistence env m) => RecordType -> [Hour] -> m ()
upsert Forecast     = insert insertForecastQ
upsert Observation  = insert insertObservationQ


prepareQ :: (WithLog env Message m, WithPersistence env m) => Query -> m ()
prepareQ q = do
    Persistence conn <- asks getPersistence
    liftIO $ execute_ conn q

prepare :: (WithLog env Message m, WithPersistence env m) => RecordType -> m ()
prepare Forecast    = prepareQ createForecastQ
prepare Observation = prepareQ createObservationsQ

initPersistence :: (MonadIO m) => String -> m Persistence
initPersistence str = do
    conn <- liftIO $ open str
    return $ Persistence conn

