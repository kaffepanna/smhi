{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Time
import Database.Beam

data HourT f = Hour { time :: C f UTCTime
                    , start :: C f UTCTime
                    , stop :: C f UTCTime
                    , temperature :: C f Double
                    , precipitation :: C f Double
                    , direction :: C f Int
                    , speed :: C f Double } deriving (Generic, Beamable)
type Hour = HourT Identity

instance Table HourT where
    data PrimaryKey HourT f = HourNoId deriving (Generic, Beamable)
    primaryKey _ = HourNoId



type ForecastT = HourT
type Forecast = ForecastT Identity

type ObservationT = HourT
type Observation = ObservationT Identity

data RecordType = Forecasts | Observations

data MAE = MAE { maeAge :: Int
               , maeError :: Double
               , maeMax :: Double
               , maeMin :: Double } deriving (Show)
