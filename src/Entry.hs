module Entry
    (main) where

import           Config
import           Interp
import           Lib

import           Data.Semigroup        ((<>))
import           Options.Applicative
import           System.Directory
import           System.FilePath.Posix

config :: FilePath -> Parser Config
config defdir = Config
    <$> strOption
        ( long "dir"
       <> short 'd'
       <> help "File path of the LaTeX file"
       <> value defdir)

configWithInfo :: FilePath -> ParserInfo Config
configWithInfo defdir =
    info (config defdir <**> helper)
        ( fullDesc
       <> header "Plotex - a tool for Practical mathematicians")

main :: IO ()
main = do
    defaultPath <- getCurrentDirectory
    con <- execParser $ configWithInfo defaultPath
    let filePath = getPath con
    isPath <- doesPathExist filePath
    isDirecotry <- doesDirectoryExist filePath
    case isPath of
        True  -> createGraphicDirectory isDirecotry filePath >> evalLatex con
        False -> putStrLn "Please Input a Valid Path"

createGraphicDirectory :: Bool -> FilePath -> IO ()
createGraphicDirectory _ _ = return ()
createGraphicDirectory isDir sourcePath = do
    case isDir of
        True  -> createHelper (sourcePath </> pGRAPHICPATHNAME)
        False -> createHelper (takeDirectory sourcePath </> pGRAPHICPATHNAME)
    where
        createHelper path = do
            isExist <- doesDirectoryExist path
            case isExist of
                True  -> removeDirectoryRecursive path >> createDirectory path
                False -> createDirectory path
