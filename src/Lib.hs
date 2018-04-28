{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( renderLatex
    ) where

import Interp
import Uid
import Config

import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Control.Monad.State
import Control.Monad.Reader


printLatex :: Either ParseError LaTeX -> IO ()
printLatex (Left _error) = return ()
printLatex (Right ast)   = print ast

renderLatex :: Program ()
renderLatex = do
    filePath <- getPath <$> ask
    latex <- liftIO $ parseLaTeXFile filePath
    liftIO $ printLatex latex
    let latex' = fmap evalLatex latex
    iolatex <- case latex' of
        Left err -> (return . TeXRaw . fromString . show) err
        Right iolatex -> iolatex 
    liftIO $ renderFile filePath iolatex

evalLatex :: LaTeX -> Program LaTeX
evalLatex latex = evalStateT (processLatex latex) 0

processLatex :: LaTeX -> StateT Int Program LaTeX
processLatex = traverseLatex (+1) processLatexHelper 

processLatexHelper :: Int -> LaTeX -> Program LaTeX
processLatexHelper sta (TeXComm "plot" [FixArg (TeXRaw dsl)]) =
    processDSL (generateUID sta) dsl

traverseLatex :: Monad m => (s -> s) -> (s -> LaTeX -> m LaTeX) -> LaTeX -> StateT s m LaTeX
traverseLatex update f tex@(TeXComm "plot" _args) = do
    sta <- get
    modify update
    lift $ f sta tex
traverseLatex update f (TeXSeq first second) = do
    first' <- traverseLatex update f first
    second' <- traverseLatex update f second
    return (TeXSeq first' second')
traverseLatex update f (TeXEnv str args latex) = do
    latex' <- traverseLatex update f latex
    return $ TeXEnv str args latex'
traverseLatex _ _ x = return x

-- processLatex :: LaTeX -> IO LaTeX
-- processLatex (TeXSeq first second) = do
--     first' <- processLatex first
--     second' <- processLatex second
--     return (TeXSeq first' second')
-- processLatex (TeXEnv str args latex) = do
--     latex' <- processLatex latex
--     return (TeXEnv str args latex')
-- processLatex (TeXComm "plot" [FixArg (TeXRaw dsl)]) = do
--     fileName <- processDSL dsl
--     return (TeXComm "plot" [FixArg (TeXRaw fileName)])
-- processLatex x = return x

-- processDSL :: T.Text -> IO T.Text
-- processDSL text = do
--     print text 
--     return "hello.txt"

-- func :: (Double -> Double) -> Double -> Double -> Double -> [(Double, Double)]
-- func f start end gap = do
--     x <- [start, start + gap .. end]
--     return (x, f x)

-- someFuncHelper :: String -> IO ()
-- someFuncHelper name = do
--     path <- getCurrentDirectory
--     toFile def (path ++ "/" ++ name ++ ".png") $ do
--         setColors [opaque blue]
--         plot (line "" [func (\x -> x * x) 1 50 0.5])

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