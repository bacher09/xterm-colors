module Main where
import Criterion.Main
import System.Console.Xterm.Colors
import System.Console.Xterm.NearestColor


main :: IO ()
main = defaultMain [
        bgroup "simple-nearest"
            [ bench "#b891f2" $ whnf simpleNearestXtermColor (mkRGB 0xB8 0x91 0xF2) 
            , bench "#ffffff" $ whnf simpleNearestXtermColor (mkRGB 0xFF 0xFF 0xFF)]
        ]
