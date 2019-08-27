module Env where

import Data.Maybe
import System.Environment

fromEnv :: String -> String -> IO String
fromEnv d = fmap (fromMaybe d) . lookupEnv


