module Main where

import Test.Hspec
import Test.Hspec.Core.Runner
import Data.Monoid

conf :: Config
conf = defaultConfig
     { configColorMode = ColorAlways }

main :: IO ()
main = hspecWith conf $ do
    describe "test tests" $ do
        it "tests if 1 is 1" $ do
            1 `shouldBe` 1
            1 `shouldBe` 2
