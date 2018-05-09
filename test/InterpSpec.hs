{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module InterpSpec where
import           Data.Either
import           Interp
import qualified Math                          as M
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec = do
    testParsePCom
    testParsePlotRange
    testParsePRange
    testParseSinglePlot
    testParsePSeq
    testParsePlotSL

helper parser = parse parser ""

testParsePCom :: Spec
testParsePCom = describe "ParserPCom" $ do
    it "parse simple pcom with default config" $
        helper parsePCom "plot(x)" `shouldBe` (Right $ PCom (M.Var "x") defaultConfig)

    it "parse pcom and set plotConfig's range field" $
        helper parsePCom "plot( x , ( 2, 4 ) )" `shouldBe` (Right $ PCom (M.Var "x") defaultConfig { range = (2, 4) })

    it "parse pcom and set range and style field" $
        helper parsePCom "plot(x, style=*** , (2, 8))" `shouldBe` (Right $ PCom (M.Var "x")
        defaultConfig { range = (2, 8), style = "***" })

    it "parse pcom and set range and style field" $
        helper parsePCom "plot(x, (2, 8), style=***  )" `shouldBe` (Right $ PCom (M.Var "x")
        defaultConfig { range = (2, 8), style = "***" })

testParsePRange :: Spec
testParsePRange = describe "ParseParsePrange" $ do
    it "parse for plotSL" $
        helper parsePRange "for a = 3:8\n\tplot(x) end" `shouldBe` (Right rangePlotSL)
            where prange = PFor 3 8 $ M.Var "a"
                  plotsl = PCom (M.Var "x") defaultConfig
                  rangePlotSL = PRange prange plotsl

testParseSinglePlot :: Spec
testParseSinglePlot = describe "ParseSinglePlot" $ do
    it "parse for SinglePlot: Single" $
        helper parseSinglePlot "plot(x, style=*** , (2, 8))" `shouldBe` (Right $ PCom (M.Var "x") defaultConfig { range = (2, 8), style = "***" })

    it "parse for SinglePlot: Range" $
        helper parseSinglePlot "for a = 3:8\n\tplot(x)\nend" `shouldBe` (Right rangePlotSL)

    where prange = PFor 3 8 $ M.Var "a"
          plotsl = PCom (M.Var "x") defaultConfig
          rangePlotSL = PRange prange plotsl

testParsePSeq :: Spec
testParsePSeq = describe "ParsePSeq" $ do
    it "parse pseq: simple" $
        helper parsePSeq "plot(x)\nplot(x)" `shouldBe` (Right resultSimple1)

    it "parse pseq: simple2" $
        helper parsePSeq "plot(x)\nfor a=1:10\n    plot(x)\nend" `shouldBe`
            (Right $ resultSimple2)

    it "parse pseq: mid" $
        helper parsePSeq "plot(x)\nplot(x)\nplot(x)\n\n" `shouldBe` (Right $ resultMid1)

    it "parse pseq: mid2" $
        helper parsePSeq "for a=1:10\n    plot(x)\nend for a=1:10\n    plot(x)\nend" `shouldBe`
        (Right $ PSeq rangeplot rangeplot)

    where varx = M.Var "x"
          plotx = PCom varx defaultConfig
          prange  = PFor 1 10 $ M.Var "a"
          rangeplot = PRange prange plotx
          resultSimple1 = PSeq plotx plotx
          resultSimple2 = PSeq plotx rangeplot
          resultMid1 = PSeq plotx resultSimple1

testParsePlotRange :: Spec
testParsePlotRange = describe "ParsePlotRange" $ do
    it "parse simple for command" $
        helper parsePlotRange "for  a = 3:8" `shouldBe` (Right $ PFor 3 8 (M.Var "a"))

testParsePlotSL :: Spec
testParsePlotSL = describe "ParserPlotSL" $ do
    it "parse plotsl: complex" $
        helper parsePlotSL "for a=1:10\n plot(x+4, (2,5))   plot(x)\nend" `shouldSatisfy` isPRange

    it "parse plotsl: complex1" $
        helper parsePlotSL "plot(x) for a = 1:10\n \tplot(x) \nend plot(x)  " `shouldBe`
        (Right $ resultComplex)

    it "parse plotsl: complex2" $
        helper parsePlotSL "for a = 1:10\nplot(x) plot(x)\nend\nplot(x) plot(x)" `shouldBe`
        (Right $ resultComplex2)

    where varx = M.Var "x"
          plotx = PCom varx defaultConfig
          prange  = PFor 1 10 $ M.Var "a"
          rangeplot = PRange prange plotx
          resultSimple = PSeq rangeplot plotx
          resultSimple2 = PSeq plotx plotx
          resultSimple3 = PRange prange resultSimple2
          resultComplex = PSeq plotx resultSimple
          resultComplex2 = PSeq resultSimple3 resultSimple2

isPSeq :: Either a PlotSL -> Bool
isPSeq (Right (PSeq _ _)) = True
isPSeq _                  = False

isPRange :: Either a PlotSL -> Bool
isPRange (Right (PRange _ _)) = True
isPRange _                    = False

isPCom :: Either a PlotSL -> Bool
isPCom x = not (isPSeq x || isPRange x)
