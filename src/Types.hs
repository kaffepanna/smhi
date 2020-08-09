{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where
import Data.Time 
import Data.Int (Int32)
import Data.Text
import Database.Beam

data StationT f = Station { stationId :: C f Int32
                          , stationName :: C f Text
                          } deriving (Generic, Beamable)

type Station = StationT Identity

                          
data HourT f = Hour { hourTime :: C f UTCTime
                    , hourStart :: C f UTCTime
                    , hourStop :: C f UTCTime
                    , hourTemperature :: C f Double
                    , hourPrecipitation :: C f Double
                    , hourDirection :: C f Int
                    , hourSpeed :: C f Double
                    } deriving (Generic, Beamable)
type Hour = HourT Identity

instance Table HourT where
    data PrimaryKey HourT f = HourNoId deriving (Generic, Beamable)
    primaryKey _ = HourNoId

instance Table StationT where
  data PrimaryKey StationT f = StationId (C f Int32) deriving (Generic, Beamable)
  primaryKey = StationId . stationId

type ForecastT = HourT
type Forecast = ForecastT Identity

type ObservationT = HourT
type Observation = ObservationT Identity

data SmhiDb f = SmhiDb { smhiForecasts :: f (TableEntity ForecastT)
                       , smhiObservations :: f (TableEntity ObservationT)
                       } deriving (Generic, Database be)

data RecordType = Forecasts | Observations

data MAE = MAE { maeAge :: Int
               , maeError :: Double
               , maeMax :: Double
               , maeMin :: Double } deriving (Show)


