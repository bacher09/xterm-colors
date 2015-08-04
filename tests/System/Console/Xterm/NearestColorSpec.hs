module System.Console.Xterm.NearestColorSpec (
    spec
) where


import Text.Printf
import Control.Monad (forM_)

import Test.Hspec
import Test.QuickCheck

import System.Console.Xterm.Colors
import System.Console.Xterm.NearestColor


hexRGB :: RGB -> String
hexRGB (RGB (r, g, b)) = printf "#%02x%02x%02x" r g b


spec :: Spec
spec = do
    -- test with pregenerated values from
    -- http://xonotic.in.ua/slava/color/term_test1.txt
    -- (color, simple, cie76)
    let testVals =
            [ (mkRGB 226 70 147, 168, 205)
            , (mkRGB 144 211 52, 113, 112)
            , (mkRGB 195 174 222, 146, 182)
            , (mkRGB 25 29 4, 233, 234)
            ]

    describe "simpleNearestColor" $ do
        forM_ testVals $ \(col, simple, cie) -> do
            let name = printf "%s should be %d" (hexRGB col) simple
            it name $ simpleNearestColor col `shouldBe` simple

        it "xterm color is nearest for itself" $ do
            property $ sameColor simpleNearestColor

    describe "cie76NearestColor" $ do
        forM_ testVals $ \(col, simple, cie) -> do
            let name = printf "%s should be %d" (hexRGB col) cie
            it name $ cie76NearestColor col `shouldBe` cie

        it "xterm color is nearest for itself" $ do
            property $ sameColor cie76NearestColor
  where
    sameColor nearestFun colInd = color == getXtermColor (nearestFun color)
      where
        color = getXtermColor colInd
