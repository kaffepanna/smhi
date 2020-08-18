module Lib
     where

import Data.Text
import Data.Maybe


(<+>) :: Text -> IO (Maybe Text) -> IO (Maybe Text)
t <+> imt = fmap (t <>) <$> imt
