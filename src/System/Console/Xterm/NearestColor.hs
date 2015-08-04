{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.NearestColor
    ( simpleNearestColor
    , cie76NearestColor
    , xterm256to16
    ) where


import Data.Word
import Data.Function (on)
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed ((!))

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


-- | This table are based on tmux's 256 color mapping to 16 color
-- table https://github.com/tmux/tmux/blob/master/colour.c#L475
-- with some minor modifications
xterm256to16Table :: VU.Vector Word8
xterm256to16Table = VU.fromList
    [  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 -- 0
    ,  0,  4,  4,  4, 12, 12,  2,  6,  4,  4, 12, 12,  2,  2,  6,  4 -- 16
    , 12, 12,  2,  2,  2,  6, 12, 12, 10, 10, 10, 10, 14, 12, 10, 10 -- 32
    , 10, 10, 10, 14,  1,  5,  4,  4, 12, 12,  3,  8,  4,  4, 12, 12 -- 48
    ,  2,  2,  6,  4, 12, 12,  2,  2,  2,  6, 12, 12, 10, 10, 10, 10 -- 64
    , 14, 12, 10, 10, 10, 10, 10, 14,  1,  1,  5,  4, 12, 12,  1,  1 -- 80
    ,  5,  4, 12, 12,  3,  3,  8,  4, 12, 12,  2,  2,  2,  6, 12, 12 -- 96
    , 10, 10, 10, 10, 14, 12, 10, 10, 10, 10, 10, 14,  1,  1,  1,  5 -- 112
    , 12, 12,  1,  1,  1,  5, 12, 12,  1,  1,  1,  5, 12, 12,  3,  3 -- 128
    ,  3,  7, 12, 12, 10, 10, 10, 10, 14, 12, 10, 10, 10, 10, 10, 14 -- 144
    ,  9,  9,  9,  9, 13, 13,  9,  9,  9,  9, 13, 13,  9,  9,  9,  9 -- 160
    , 13, 12,  9,  9,  9,  9, 13, 12, 11, 11, 11, 11,  7, 12, 11, 11 -- 176
    , 10, 10, 10, 14,  9,  9,  9, 13, 13, 13,  9,  9,  9,  9,  9, 13 -- 192
    ,  9,  9,  9,  9,  9, 13,  9,  9,  9,  9,  9, 13, 11, 11, 11,  9 -- 208
    ,  9, 15, 11, 11, 11, 11, 15, 15,  0,  0,  0,  0,  0,  0,  8,  8 -- 224
    ,  8,  8,  8,  8,  7,  7,  7,  7,  7,  7, 15, 15, 15, 15, 15, 15 -- 240
    ]

-- | Convert xterm 256 color to ansii 16 color
xterm256to16 :: Word8 -> Word8
xterm256to16 colorIndex = xterm256to16Table ! (fromIntegral colorIndex)
