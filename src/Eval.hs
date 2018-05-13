module Eval where
import           Config
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Map.Strict                        as Map
import qualified Data.Text                              as T
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import qualified Math                                   as M
import           Source
type Env = Map.Map String Double
type Eval = ReaderT Env (WriterT [[(Double, Double)]] (ExceptT String Identity))

interpDSL :: FilePath -> T.Text -> Program ()
interpDSL filepath t = liftIO $ do
    print t
    case plotSLparse t of
        Left err  -> print err
        Right val -> liftIO $ print val >> produceImage filepath val

produceImage ::FilePath -> PlotSL -> IO ()
produceImage path psl = join $ fmap (produceImageHelper path . combineToEC) $ runEval empty (eval psl)

runEval :: Env -> Eval a -> IO [[(Double, Double)]]
runEval env ev =
    case result of
        Left err           -> print err >> return []
        Right (_, results) -> return results
    where result = runIdentity $ runExceptT $ runWriterT $ runReaderT ev env

produceImageHelper :: (Default r, ToRenderable r) => FilePath -> EC r () -> IO ()
produceImageHelper = toFile def

eval :: PlotSL -> Eval ()
eval (PCom expr config) = do
    env <- ask
    let numbers = rangeToList $ range config
    if numbers == [] then
        throwError "empty x range"
    else
        case evalExpr env numbers expr of
            Just value -> tell [value]
            Nothing    -> throwError ("Invalid Numerical Expr : " ++ show expr) -- tell []

eval (PRange prange psl) = sequence_ $ fmap (flip evalRangeHelper psl) (evalPlotRange prange)

eval (PSeq first second) = do
    eval first >> eval second

evalRangeHelper :: (String, Double) -> PlotSL -> Eval ()
evalRangeHelper (name, value) plotsl = local (\env -> updateVarInEnv env name value) $ eval plotsl

rangeToList :: (Integer, Integer) -> [Double]
rangeToList (l, u) = [fromIntegral l, fromIntegral l + 0.5 .. fromIntegral u]

evalPlotRange :: PlotRange -> [(String, Double)]
evalPlotRange (PFor l u (M.Var name)) = fmap ((,) name ) [fromIntegral l .. fromIntegral u]
evalPlotRange _ = []

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

plotLine :: [(Double, Double)] -> EC (Layout Double Double) ()
plotLine xs = plot (line ""  [xs])

combineToEC :: [[(Double, Double)]] -> EC (Layout Double Double) ()
combineToEC = (sequence_ . fmap plotLine)

-- signal :: [Double] -> [(Double,Double)]
-- signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

-- signal2 :: [Double] -> [(Double,Double)]
-- signal2 xs = [ (x,(cos (x*3.14159/45) + 1) / 2 * (cos (x*3.14159/5))) | x <- xs ]

-- main = toFile def "example.png" $ do
--     layout_title .= "Amplitude Modulation"
--     plot (line "am" [signal [0,(0.5)..400]])
--     plot (line "am" [signal2 [0,(0.5)..400]])
