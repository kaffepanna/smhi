module Types where

import Data.Time

data Hour = Hour { approvedTime :: UTCTime
                 , accStart :: UTCTime
                 , accEnd :: UTCTime
                 , temperature :: Double
                 , perspiration :: Double
                 } deriving (Show)

data MAE = MAE { age :: Int
               , nSamples :: Int
               , temperatureAE :: Double
               , perspirationAE :: Double
               } deriving (Show)

data HourOffset = HourOffset { hoursAgo :: Int
                             , temperatureE :: Double
                             , perspirationE :: Double
                             } deriving (Show)

data RecordType = Forecast | Observation
