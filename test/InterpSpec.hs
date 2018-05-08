{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module InterpSpec where
import           Interp
import qualified Math                          as M
import           Test.Hspec
import           Text.ParserCombinators.Parsec

spec :: Spec
spec = testParsePCom
-- hello :: Spec
-- hello = describe "head" $ do
--     it "returns the first element of a list" $
--         head [23 ..] `shouldBe` 23
--     it "returns the first element of a list" $
--         head [23 ..] `shouldBe` 23

helper parser = parse parser ""

testParsePCom :: Spec
testParsePCom = describe "ParserPCom" $ do
    it "parse simple pcom with default config" $
        helper parsePCom "plot(x)" `shouldBe` (Right $ PCom (M.Var "x") defaultConfig)

    it "parse pcom and set plotConfig's range field" $
        helper parsePCom "plot(x, [2, 4])" `shouldBe` (Right $ PCom (M.Var "x") defaultConfig { range = (2, 4) })
