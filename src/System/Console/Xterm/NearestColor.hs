{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.NearestColor
    ( simpleNearestColor
    , cie76NearestColor
    ) where


import Data.Word
import Data.Function (on)
import qualified Data.Vector.Unboxed as VU

import System.Console.Xterm.Types
import System.Console.Xterm.Colors
import System.Console.Xterm.Colors.TH (unLAB, rgbToLabd65)


-- | search nearest xterm color for 24-bit RGB color using
-- simple Euclidean distance
-- Complexity: O(n)
simpleNearestColor :: RGB -> Word8
simpleNearestColor color = fromIntegral nearestIndex
  where
    nearestIndex = VU.minIndexBy (compare `on` rgbdist color) xtermColors
    -- dist without sqrt for performance
    rgbdist :: RGB -> RGB -> Int
    rgbdist (RGB (r1, g1, b1)) (RGB (r2, g2, b2)) = dist
                (fromIntegral r1) (fromIntegral g1) (fromIntegral b1)
                (fromIntegral r2) (fromIntegral g2) (fromIntegral b2)
    dist x1 y1 z1 x2 y2 z2 = (x1 - x2)^(2 :: Int) +
                             (y1 - y2)^(2 :: Int) +
                             (z1 - z2)^(2 :: Int)


-- xterm colors in lab color space converted in compile time using
-- template haskell
xtermColorsLab :: VU.Vector FloatLAB
xtermColorsLab = $(let val = VU.map (unLAB . rgbToLabd65) xtermColors
                   in [| val |])


-- | search nearest xterm color for 24-bit RGB color using
-- CIE76 ΔE (Delta E)
-- Complexity: O(n)
cie76NearestColor :: RGB -> Word8
cie76NearestColor color = fromIntegral nearestIndex
  where
    nearestIndex = VU.minIndexBy (compare `on` fLabDist flabColor) xtermColorsLab
    flabColor = convert color
    convert = unLAB . rgbToLabd65
    fLabDist :: FloatLAB -> FloatLAB -> LABDistance
    fLabDist (l1, a1, b1) (l2, a2, b2) = dist l1 a1 b1 l2 a2 b2
    dist x1 y1 z1 x2 y2 z2 = (x1 - x2)^(2 :: Int) +
                             (y1 - y2)^(2 :: Int) +
                             (z1 - z2)^(2 :: Int)
