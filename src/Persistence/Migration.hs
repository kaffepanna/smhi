{-# LANGUAGE OverloadedStrings #-}
module Persistence.Migration where

import Types
import Data.Time
import Database.Beam.Migrate
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.Beam.Query.DataTypes

-- It's unfortunate that we have to define this ourselves.
utctime :: BeamSqlBackend be => DataType be UTCTime
utctime = DataType (timestampType Nothing False)


initialSetup :: Migration Postgres (CheckedDatabaseSettings Postgres SmhiDb)
initialSetup = SmhiDb <$> (createTable "forecasts" $ Hour { hourTime  = field "time" utctime unique
                                                          , hourStart = field "start" utctime unique
                                                          , hourStop  = field "stop" utctime unique
                                                          , hourTemperature = field "temperature" double
                                                          , hourPrecipitation = field "precipitation" double
                                                          , hourDirection = field "direction" int
                                                          , hourSpeed = field "speed" double
                                                          })
                      <*> (createTable "observations" $ Hour { hourTime  = field "time" utctime
                                                             , hourStart = field "start" utctime unique
                                                             , hourStop  = field "stop" utctime unique
                                                             , hourTemperature = field "temperature" double
                                                             , hourPrecipitation = field "precipitation" double
                                                             , hourDirection = field "direction" int
                                                             , hourSpeed = field "speed" double
                                                             })

initialSetupStep :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres SmhiDb)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

