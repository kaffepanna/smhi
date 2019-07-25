{-# LANGUAGE OverloadedStrings
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , InstanceSigs
  , QuasiQuotes #-}
module Main where

import Data.List

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader

import Data.Maybe
import qualified Data.Vector as V
import Data.Time

import Types
import Persistence

import Colog
import Colog.Message
import Colog.Actions

import Graphics.Rendering.Chart.Easy hiding ((<.>))
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)

import System.Environment
import System.FilePath.Posix
import Statistics.Quantile

import Text.InterpolatedString.Perl6 (qc)

data Env m = Env { _persistence :: Persistence
                 , _logAction :: !(LogAction m Message) }

newtype App e = App { unapp :: ReaderT (Env App) IO e }
    deriving (Monad, Functor, Applicative, MonadIO, MonadReader (Env App))

instance HasPersistence (Env m) where
    getPersistence = _persistence

instance HasLog (Env m) Message m where
    getLogAction = _logAction
    setLogAction newLogAction env = env { _logAction = newLogAction }

indexHtml diagram = [qc|
<html>
    <head>
        <title>SMHI Forecast error</title>
    </head>
    <body>
        <center><img src="{diagram}" /></center>
    </body>
</html>
|]

withApp :: Env App -> App () -> IO ()
withApp env f = runReaderT (unapp f) env

-- environment variable with default value in io monad
fromEnv :: String -> String -> IO String
fromEnv d = fmap (fromMaybe d) . lookupEnv

type BoxValues = (Double, Double, Double, Double, Double)

lineStyle :: Double -> Colour Double -> LineStyle
lineStyle n colour = line_width .~ n
                   $ line_color .~ opaque colour
                   $ def

box label color vals = liftEC $ do
    plot_candle_line_style  .= lineStyle 1 color
    plot_candle_fill .= False
    plot_candle_rise_fill_style .= solidFillStyle (opaque color)
    plot_candle_fall_fill_style .= solidFillStyle (opaque color)
    plot_candle_tick_length .= 2
    plot_candle_width .= 8
    plot_candle_centre .= 8
    plot_candle_values .= [ Candle ind mn first med third mx | (ind,(mn,first,med,third,mx)) <- vals]
    plot_candle_title .= label


grpBox :: [HourOffset] -> (Double, (Double, Double, Double, Double, Double))
grpBox os = (k, (mn, q1, q2, q3, mx))
    where k = fromIntegral $ hoursAgo (head os) 
          mn = quantile s 1 10 temperatures
          mx = quantile s 9 10 temperatures
          q1 = quantile s 1 4 temperatures
          q2 = quantile s 2 4 temperatures
          q3 = quantile s 3 4 temperatures
          temperatures = V.fromList (map temperatureE os)

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

errorsByHour = groupBy (\a b -> hoursAgo a == hoursAgo b) . sortOn hoursAgo

main :: IO ()
main = do
    time <- show <$> getCurrentTime
    outdir <- fromEnv "./" "OUTDIR"
    database <- fromEnv "data.db" "DATABASE"
    image <- fromEnv (outdir </> time <.> "svg") "DIAGRAM"
    index <- fromEnv (outdir </> "index.html") "HTML"

    env <- Env <$> initPersistence database
               <*> pure richMessageAction

    withApp env $ do
        samples <- getHourOffsets
        let grps = errorsByHour samples 
            positives = map (filter (\a -> temperatureE a > 0)) grps
            negatives = map (filter (\a -> temperatureE a < 0)) grps
            positivesBoxes = map grpBox $ filter (\grp -> length grp > 2) positives
            negativesBoxes = map grpBox $ filter (\grp -> length grp > 2) negatives

        mae <- getMAE
        liftIO $ toFile def image $ do
            layout_title .= "SMHI Forecast Accuracy (Generated " ++ show time ++")"
            layout_y_axis . laxis_title   .= "Temperature error (C)"
            layout_x_axis . laxis_title   .= "Forecast age (h)"
            layout_x_axis . laxis_reverse .= True

            plot (box "high forecast" blue $ every 10 negativesBoxes)
            plot (box "low forecast"  red  $ every 10 positivesBoxes)

        liftIO $ writeFile index (indexHtml image) 
