module Interp
    ( processDSL
    ) where

import Config

import qualified Data.Text as T
import Text.LaTeX.Base.Syntax
import Data.String
import Control.Monad.Reader
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo

pACKAGENAME :: String
pACKAGENAME = "plotex"

processDSL :: String -> T.Text -> Program LaTeX 
processDSL uid text = do
    let filename = fromString $ pACKAGENAME ++ "_" ++ uid ++ ".png"
    interpDSL filename text
    return (TeXComm "plot" [FixArg (TeXRaw filename)])

interpDSL :: T.Text -> T.Text -> Program T.Text
interpDSL _ t = do
    con <- ask
    (liftIO . putStrLn . getPath) con
    liftIO $ print t
    return t