module Eval where
import           Config
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Map.Strict                        as Map
import qualified Data.Text                              as T
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Math                                   as M
import           Source
import           Text.ParserCombinators.Parsec
type Env = Map.Map String Double
type Eval = ReaderT Env (ExceptT String IO)

interpDSL :: FilePath -> T.Text -> Program ()
interpDSL filepath t = do
    liftIO $ print t
    case parse parsePlotSL "PlotSL" (T.unpack t) of
        Left err  -> (liftIO $ print err)
        Right val -> (produceImage filepath val)

runEval ::
  MonadIO m => r -> ReaderT r (ExceptT e IO) a -> m (Either e a)
runEval env ev = liftIO $ runExceptT $ runReaderT ev env

produceImage ::FilePath -> PlotSL -> Program ()
produceImage path plotsl = runEval (fromList []) (eval path plotsl) >> return ()

produceImageHelper :: (Default r, ToRenderable r) => FilePath -> EC r () -> IO ()
produceImageHelper = toFile def

eval :: FilePath -> PlotSL -> Eval ()
eval path (PCom expr config) = do
    env <- ask
    let numbers = rangeToList $ range config
    case evalExpr env numbers expr of
        Just value -> liftIO $ produceImageHelper path $ plotLine [value]
        Nothing    -> throwError "Invalid Numerical Expr"

rangeToList :: (Integer, Integer) -> [Double]
rangeToList (l, u) = [fromIntegral l, (0.5), fromIntegral u]

evalExpr :: Env -> [Double] -> PlotExpr -> Maybe [(Double, Double)]
evalExpr env xs expr = sequence $ fmap (evalExprHelper env expr) xs

evalExprHelper :: Env -> PlotExpr -> Double -> Maybe (Double, Double)
evalExprHelper env expr xval = do
    yval <- M.evaluate (updateVarInEnv env "x" xval) $ Just expr
    return (xval, yval)

updateVarInEnv :: Env -> String -> Double -> Env
updateVarInEnv env key val = case Map.lookup key env of
    (Just _) -> Map.update (Just . const val) key env
    Nothing  -> Map.insert key val env

plotLine :: [[(x, y)]] -> EC (Layout x y) ()
plotLine xs = plot (line "" xs)

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = toFile def "example.png" $ do
    layout_title .= "Amplitude Modulation"
    plot (line "am" [signal [0,(0.5)..400]])
    plot (points "am points" (signal [0,7..400]))
