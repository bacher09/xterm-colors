module System.Console.Xterm.ColorsSpec (
    spec
) where


import Test.Hspec
import System.Console.Xterm.Colors


spec :: Spec
spec = do
    -- compare results with table in
    -- http://www.calmar.ws/vim/256-xterm-24bit-rgb-color-chart.html
    describe "getXtermColor" $ do
        it "0 is #000000" $ do
            getXtermColor 0 `shouldBe` mkRGB 0x0 0x0 0x0

        it "15 is #ffffff" $ do
            getXtermColor 15 `shouldBe` mkRGB 0xFF 0xFF 0xFF

        it "121 is #87ffaf" $ do
            getXtermColor 121 `shouldBe` mkRGB 0x87 0xFF 0xAF

        it "236 is #303030" $ do
            getXtermColor 236 `shouldBe` mkRGB 0x30 0x30 0x30

        it "255 is #eeeeee" $ do
            getXtermColor 255 `shouldBe` mkRGB 0xEE 0xEE 0xEE
