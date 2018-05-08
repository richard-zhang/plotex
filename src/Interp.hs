module Interp
    ( processDSL
    ) where

import Config

import qualified Data.Text as T
import Text.LaTeX.Base.Syntax
import Data.String
import Control.Monad.Reader
import Text.ParserCombinators.Parsec
import qualified Math as M
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo
type PlotExpr = M.Expr Double

data PlotSL = PSeq PlotSL PlotSL
            | PRange PlotRange PlotSL
            | PCom PlotExpr PlotConfig

data PlotRange = PFor Integer Integer PlotExpr

data PlotConfig = PlotConfig { range :: (Integer, Integer), style :: String }

defaultConfig :: PlotConfig
defaultConfig = PlotConfig 
    { range = (0, 10)
    , style = "---"
    }

listOfStyle :: [String]
listOfStyle = ["---", "***"]

parseStyle :: Parser String
parseStyle = foldl1 (<|>) $ fmap string listOfStyle

digits :: Parser Integer
digits = fmap read (many1 digit)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<?>@^_~#"

parsePlotExpr :: Parser PlotExpr
parsePlotExpr = M.build

parsePlotSL :: Parser PlotSL
parsePlotSL = try parsePSeq <|> try parsePRange <|> parsePCom

parsePlotConfigRange ::Parser (PlotConfig -> PlotConfig)
parsePlotConfigRange = do
    char '['
    spaces
    [lowerBound, upperBound] <- sepBy1 digits (spaces >> char ',' >> spaces)
    spaces
    char ']'
    return $ \config -> config { range = (lowerBound, upperBound) }

parsePlotConfigStyle :: Parser (PlotConfig -> PlotConfig)
parsePlotConfigStyle = do
    string "style"
    char '='
    char '\"'
    sty <- parseStyle 
    char '\"'
    return $ \config -> config { style = sty }

parsePlotConfig ::Parser (PlotConfig -> PlotConfig)
parsePlotConfig = parsePlotConfigRange <|> parsePlotConfigStyle

parsePlotRange :: Parser PlotRange
parsePlotRange = do
    string "for"
    space
    expr <- M.variable
    spaces
    char '='
    spaces
    lowerBound <- digits
    char ':'
    upperBound <- digits
    spaces
    return $ PFor lowerBound upperBound expr

parsePRange :: Parser PlotSL
parsePRange = do
    plotRange <- parsePlotRange
    plotSL <- parsePlotSL
    string "end"
    return $ PRange plotRange plotSL

parseSinglePlot :: Parser PlotSL
parseSinglePlot = try parsePRange <|> parsePCom

parsePSeq :: Parser PlotSL
parsePSeq =
    try $ do
        first <- parseSinglePlot
        second <- parseSinglePlot
        return $ PSeq first second
    <|> do
        first <- parsePSeq
        second <- parseSinglePlot
        return $ PSeq first second

parsePCom :: Parser PlotSL
parsePCom = do
    string "plot"
    char '('
    expr <- parsePlotExpr
    spaces
    char ','
    spaces
    configs <- sepBy parsePlotConfig (spaces >> char ',' >> spaces)
    let config = foldl1 (.) configs defaultConfig
    spaces
    char ')'
    spaces
    return $ PCom expr config

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