{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import System.Directory
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T

type MyState a = StateT Int IO a

pACKAGENAME :: String
pACKAGENAME = "plotex"

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

printLatex :: Either ParseError LaTeX -> IO ()
printLatex (Left _error) = return ()
printLatex (Right ast)   = print ast

renderLatex :: IO ()
renderLatex = do
    path <- getCurrentDirectory
    let filePath = path ++ "/resources/test.tex"
    latex <- parseLaTeXFile filePath
    printLatex latex
    let latex' = fmap evalLatex latex
    iolatex <- case latex' of
        Left err -> (return . TeXRaw . fromString . show) err
        Right iolatex -> iolatex 
    renderFile filePath iolatex

evalLatex :: LaTeX -> IO LaTeX
evalLatex latex = evalStateT (processLatex' latex) 0

processLatex' :: LaTeX -> MyState LaTeX
processLatex' (TeXComm "plot" [FixArg (TeXRaw dsl)]) = do
    s <- get
    modify (+ 1)
    fileName <- liftIO $ processDSL' (letters !! s) dsl
    return (TeXComm "plot" [FixArg (TeXRaw fileName)])
processLatex' (TeXSeq first second) = do
    first' <- processLatex' first
    second' <- processLatex' second
    return (TeXSeq first' second')
processLatex' (TeXEnv str args latex) = do
    latex' <- processLatex' latex
    return (TeXEnv str args latex')
processLatex' x = return x

processDSL' :: String -> T.Text -> IO T.Text
processDSL' myid text = do
    print text
    return $ fromString $ pACKAGENAME ++ "_" ++ myid ++ ".png"

processLatex :: LaTeX -> IO LaTeX
processLatex (TeXSeq first second) = do
    first' <- processLatex first
    second' <- processLatex second
    return (TeXSeq first' second')
processLatex (TeXEnv str args latex) = do
    latex' <- processLatex latex
    return (TeXEnv str args latex')
processLatex (TeXComm "plot" [FixArg (TeXRaw dsl)]) = do
    fileName <- processDSL dsl
    return (TeXComm "plot" [FixArg (TeXRaw fileName)])
processLatex x = return x

processDSL :: T.Text -> IO T.Text
processDSL text = do
    print text 
    return "hello.txt"

func :: (Double -> Double) -> Double -> Double -> Double -> [(Double, Double)]
func f start end gap = do
    x <- [start, start + gap .. end]
    return (x, f x)

someFuncHelper :: String -> IO ()
someFuncHelper name = do
    path <- getCurrentDirectory
    toFile def (path ++ "/" ++ name ++ ".png") $ do
        setColors [opaque blue]
        plot (line "" [func (\x -> x * x) 1 50 0.5])

someFunc :: IO ()
someFunc = renderLatex
-- someFunc = H.withEmbeddedR defaultConfig $
    -- H.runRegion $ do 
        -- path <- io getCurrentDirectory
        -- [r| png(filename=paste(path_hs, "/na.png", sep=""));
            -- curve(x^2, from=1, to=50, , xlab="x", ylab="y");
            -- dev.off();
        -- |]
        -- return ()
-- someFunc = H.withEmbeddedR defaultConfig $
    -- H.runRegion $ do
        -- std <- io newStdGen
        -- let (xs::[Double]) = take 100 $ randoms std
        -- d <- [r| matrix(xs_hs, ncol=2) |]
        -- rv <- [r| clusters <- kmeans(d_hs, 2) |]
        -- [r| par(mar = c(5.1, 4.1, 0, 1));
            -- plot(d_hs, col = rv_hs$cluster, pch = 20
                -- , cex = 3, xlab = "x", ylab = "y");
            -- points(rv_hs$centers, pch = 4, cex = 4, lwd = 4);
        -- |]
        -- return ()
-- 