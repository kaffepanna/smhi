{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Persistence.Queries where

import Database.SQLite.Simple
import Text.InterpolatedString.Perl6 (q, qc)

createForecastQ :: Query
createForecastQ = [q|
CREATE TABLE IF NOT EXISTS forecasts (
    approvedTime text,
    accStart text,
    accEnd text,
    temperature numeric,
    perspiration numeric,
    unique (approvedTime, accStart, accEnd)
)
|]

createObservationsQ :: Query
createObservationsQ = [q|
CREATE TABLE IF NOT EXISTS observations (
    approvedTime text,
    accStart text,
    accEnd text,
    temperature numeric,
    perspiration numeric,
    unique (accStart, accEnd)
)
|]

insertQ :: String -> Query
insertQ table = [qc|
INSERT OR IGNORE INTO {table} (
    approvedTime,
    accStart,
    accEnd,
    temperature,
    perspiration
) VALUES (?, ? ,? ,?, ?)
|]

getHoursQ :: Query
getHoursQ = [q|
SELECT cast((julianday(forecasts.accStart) - julianday(forecasts.approvedTime)) * 24 as int) as age,
       cast((forecasts.temperature-observations.temperature) as double) as temperatureError,
       cast((forecasts.perspiration - observations.perspiration) as double) as perspirationError
FROM forecasts
INNER JOIN observations ON observations.accStart = forecasts.accStart
WHERE age > 0
|]

getMAEQ :: Query
getMAEQ = [q|
SELECT cast((julianday(forecasts.accStart) - julianday(forecasts.approvedTime)) * 24 as int) as age,
       count(*) as nSamples,
       avg(abs(forecasts.temperature-observations.temperature)) as temperatureError,
       avg(abs(forecasts.perspiration - observations.perspiration)) as perspirationError
FROM forecasts
INNER JOIN observations ON observations.accStart = forecasts.accStart
WHERE age > 0
GROUP BY age;
|]

insertForecastQ :: Query
insertForecastQ =  insertQ "forecasts"
insertObservationQ :: Query
insertObservationQ = insertQ "observations"
