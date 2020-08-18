module Env where

import Data.Text as T
import Data.Maybe
import System.Environment as E

getEnv :: Text -> IO (Maybe Text)
getEnv t = E.lookupEnv (unpack t) >>= \mt -> pure $ pack <$> mt

