module Config where

import           Control.Monad.Reader
import           Control.Monad.State

data Config = Config { getPath :: FilePath } deriving (Show)
type Program = ReaderT Config (StateT Int IO)
