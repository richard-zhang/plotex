module Config where

import Control.Monad.Reader

data Config = Config { getPath :: FilePath } deriving (Show)
type Program = ReaderT Config IO