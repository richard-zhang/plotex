{-# LANGUAGE OverloadedStrings #-}
module Interp where

import           Config
import           Control.Monad.Reader
import           Data.String
import qualified Data.Text              as T
import           Eval
import           System.FilePath.Posix
import qualified Text.LaTeX.Base.Render as L
import           Text.LaTeX.Base.Syntax

pGRAPHICPATHNAME :: String
pGRAPHICPATHNAME = "plotexfig"

convertToLatexComment :: [TeXArg] -> LaTeX
convertToLatexComment args = mconcat comments
    where
        texts = T.lines $ L.render $ TeXComm "plot" args
        comments = fmap TeXComment texts

processDSL :: String -> LaTeX -> Program LaTeX
processDSL uid (TeXComm "plot" args) = do
    let comment = convertToLatexComment args
    latex <- processDSLHelper uid args
    return $ TeXSeq comment latex

processDSLHelper :: String -> [TeXArg] -> Program LaTeX
processDSLHelper uid (FixArg (TeXRaw text):args) = do
    figPath <- asks getPath
    let name = uid ++ ".png"
    let filename = pGRAPHICPATHNAME </> name
    let filepath = figPath </> filename
    interpDSL filepath text
    return (TeXComm "includegraphics" (FixArg (TeXRaw $ fromString filename):args))

processDSLHelper uid (x:xs) = do
    (TeXComm com args) <- processDSLHelper uid xs
    return $ TeXComm com (x:args)

processDSLHelper _ [] = do
    liftIO $ putStrLn "please input a valid DSL"
    return (TeXComm "plot" [])
