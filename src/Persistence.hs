{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds, DeriveAnyClass, DeriveGeneric, TypeApplications, OverloadedStrings #-}
module Persistence where

import Prelude hiding (log)

import Control.Monad.Reader
import Control.Monad (forM_)
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Full as Pgf
import Database.Beam as B
import Database.Beam.Backend.SQL
import Colog

import Data.Maybe
import Data.Time
import Data.Text (pack)
import Types as T
import Network

newtype Persistence = Persistence { _runBeam :: forall a. Pg.Pg a -> IO a }

type WithBeam env m = (MonadReader env m, MonadIO m, HasBeam env)

class HasBeam env where
    getPersistence :: env -> Persistence

instance HasBeam Persistence where
    getPersistence = id

data SmhiDb f = SmhiDb { smhiForecasts :: f (TableEntity ForecastT)
                       , smhiObservations :: f (TableEntity ObservationT)
                       } deriving (Generic, Database be)

smhiDb :: DatabaseSettings be SmhiDb
smhiDb = defaultDbSettings

upsertForecasts :: (WithLog env Message m, WithBeam env m) => [Forecast] -> m ()
upsertForecasts forecasts = do
   Persistence runBeam <- asks getPersistence
   liftIO . runBeam . runInsert $
       Pgf.insert (smhiForecasts smhiDb) (insertValues forecasts) $
           Pgf.onConflict
            Pgf.anyConflict
            Pgf.onConflictDoNothing

upsertObservations :: (WithLog env Message m, WithBeam env m) => [Observation] -> m ()
upsertObservations observations = do
   Persistence runBeam <- asks getPersistence
   liftIO . runBeam . runInsert $
       Pgf.insert (smhiObservations smhiDb) (insertValues observations) $
           Pgf.onConflict
            Pgf.anyConflict
            Pgf.onConflictDoNothing

hoursSinceEpoch_ c = valueExpr_ $ as_ @Int $ customExpr_ (\t -> "CAST (EXTRACT(EPOCH FROM " <> t <> ") AS Int)/3600") c

getMAE :: (WithLog env Message m, WithBeam env m) => m [MAE]
getMAE = do
    Persistence runBeam <- asks getPersistence
    r <- liftIO . runBeam . runSelectReturningList . select $
        aggregate_ (\(obs, fcs) -> let age = group_ $ hoursSinceEpoch_ (start obs) - hoursSinceEpoch_ (T.time fcs)
                                       err = fromMaybe_ 0 (avg_ $ abs (temperature fcs - temperature obs))
                                       mn  = fromMaybe_ 0 (min_ $ abs (temperature fcs - temperature obs))
                                       mx  = fromMaybe_ 0 (max_ $ abs (temperature fcs - temperature obs))

                                    in (age, err, mx, mn)) $ do
            fcs <- all_ (smhiForecasts smhiDb)
            obs <- all_ (smhiObservations smhiDb)

            guard_ (start obs ==. start fcs)
            pure (obs, fcs)
    return $ uncurry4 MAE <$> r

uncurry4 f (a,b,c,d) = f a b c d
