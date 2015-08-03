module System.Console.Xterm.NearestColor
    ( simpleNearestColor
    ) where


import Data.Word
import Data.Function (on)
import qualified Data.Vector.Unboxed as VU
import System.Console.Xterm.Types
import System.Console.Xterm.Colors


-- using float to reduce memory usage
type FloatLAB = (Float, Float, Float)
type LABDistance = Float


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
