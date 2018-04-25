module Interp
    ( processDSL
    ) where

import qualified Data.Text as T
import Text.LaTeX.Base.Syntax
import Data.String

pACKAGENAME :: String
pACKAGENAME = "plotex"

processDSL :: String -> T.Text -> IO LaTeX
processDSL uid text = do
    let filename = fromString $ pACKAGENAME ++ "_" ++ uid ++ ".png"
    interpDSL filename text
    return (TeXComm "plot" [FixArg (TeXRaw filename)])

interpDSL :: T.Text -> T.Text -> IO T.Text
interpDSL _ t = do
    print t
    return t