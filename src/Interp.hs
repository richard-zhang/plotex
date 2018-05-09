{-# LANGUAGE OverloadedStrings #-}
module Interp where

import           Config

import           Control.Monad.Reader
import           Data.String
import qualified Data.Text                              as T
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Math                                   as M
import           Text.LaTeX.Base.Syntax
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token    as P
type PlotExpr = M.Expr Double

data PlotSL = PSeq PlotSL PlotSL
            | PRange PlotRange PlotSL
            | PCom PlotExpr PlotConfig
            deriving (Show, Eq)

data PlotRange = PFor Integer Integer PlotExpr deriving (Show, Eq)

data PlotConfig = PlotConfig { range :: (Integer, Integer), style :: String } deriving (Show, Eq)

lexer       = P.makeTokenParser haskellDef
parens      = P.parens lexer
symbol      = P.symbol lexer
comma       = P.comma lexer
commaSep    = P.commaSep lexer
squares     = P.squares lexer

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

parsePlotExpr :: Parser PlotExpr
parsePlotExpr = M.build

parsePlotSL :: Parser PlotSL
parsePlotSL = try parsePSeq <|> try parsePRange <|> parsePCom

parsePlotConfigRange ::Parser (PlotConfig -> PlotConfig)
parsePlotConfigRange = squares $ do
    spaces
    [lowerBound, upperBound] <- sepBy1 digits $ try (spaces >> comma)
    spaces
    return $ \config -> config { range = (lowerBound, upperBound) }

parsePlotConfigStyle :: Parser (PlotConfig -> PlotConfig)
parsePlotConfigStyle = do
    string "style"
    char '='
    optional $ char '\"'
    sty <- parseStyle
    optional $ char '\"'
    return $ \config -> config { style = sty }

parsePlotConfig ::Parser (PlotConfig -> PlotConfig)
parsePlotConfig = parsePlotConfigRange <|> parsePlotConfigStyle

parsePlotRange :: Parser PlotRange
parsePlotRange = do
    string "for"
    skipMany1 space
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
    spaces
    return $ PRange plotRange plotSL

parseSinglePlot :: Parser PlotSL
parseSinglePlot = (try parsePRange) <|> parsePCom

parsePSeq :: Parser PlotSL
parsePSeq = do
    first <- parseSinglePlot
    second <- try parsePSeq <|> parseSinglePlot
    return $ PSeq first second

parsePCom :: Parser PlotSL
parsePCom =
    string "plot" >>
    parens (do
        expr <- parsePlotExpr
        spaces
        optional comma
        configs <- sepBy parsePlotConfig $ try (spaces >> comma)
        let config = foldl (.) id configs defaultConfig
        spaces
        return $ PCom expr config
        )

pACKAGENAME :: String
pACKAGENAME = "plotex"

processDSL :: String -> [TeXArg] -> Program LaTeX
processDSL uid (FixArg (TeXRaw text):args) = do
    figPath <- asks getPath
    -- (liftIO . putStrLn . getPath) con
    let filename = fromString $ figPath ++ "/" ++ pACKAGENAME ++ "_" ++ uid ++ ".png"
    liftIO $ print text
    -- interpDSL filename text
    return (TeXComm "includegraphics" (FixArg (TeXRaw filename):args))

processDSL uid (x:xs) = do
    (TeXComm com args) <- processDSL uid xs
    return $ TeXComm com (x:args)

processDSL _ [] = do
    liftIO $ putStrLn "please input a valid DSL"
    return (TeXComm "plot" [])

produceImage :: T.Text -> PlotSL -> Program ()
produceImage = undefined

interpDSL :: T.Text -> T.Text -> Program ()
interpDSL abspath t = do
    liftIO $ print t
    case parse parsePlotSL "PlotSL" (T.unpack t) of
        Left err  -> (liftIO $ print err)
        Right val -> (produceImage abspath val)
