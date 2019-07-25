{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Data.Time
import Types
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import Control.Monad.Extra (concatMapM)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy as BS

hourParser :: UTCTime -> Value -> Parser Hour
hourParser app = withObject "Hour" $ \o -> do
    as <- o .: "accStart"
    ae <- o .: "accEnd"
    temp  <- read <$> o .: "t"
    pers <- o .: "tp" <|> o .: "prec1h"
    pure $ Hour app as ae temp (read pers)

hoursParser app = withArray "data" $ mapM (hourParser app) . V.toList

serieParser app = withObject "serie" $ \o -> o .: "data" >>= hoursParser app 

seriesParser app = withArray "series" $ concatMapM (serieParser app) . V.toList

rootParser = withObject "root" $ \o -> do
    approvedTime <- o .: "approvedTime" :: Parser UTCTime
    o .: "daySerie" >>= seriesParser approvedTime

parseHours :: BS.ByteString -> Maybe [Hour]
parseHours bs = parseMaybe rootParser =<< ast
    where ast = decode bs :: Maybe Value
