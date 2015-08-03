module System.Console.Xterm.NearestColor
    ( simpleNearestColor
    , cie76NearestColor
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

-- | Convert 24-bit RGB to XYZ using CIE profile
rgbToXYZWithProfile :: CIEProfile -> RGB -> XYZ
rgbToXYZWithProfile prof rgb = XYZ (x, y, z)
  where
    RGB (r, g, b) = rgb
    (rd, gd, bd) = (convert r, convert g, convert b)
    CIEProfile (i1, i2, i3) (j1, j2, j3) (f1, f2, f3) = prof
    x = i1*rd + i2*gd + i3*bd
    y = j1*rd + j2*gd + j3*bd
    z = f1*rd + f2*gd + f3*bd
    convert :: Word8 -> Double
    convert = pivot . (/255) . fromIntegral
    pivot v = if v > 0.04045
        then ((v + 0.055) / 1.055) ** 2.4
        else v / 12.92


-- | Convert XYZ to LAB using specific white reference
xyzToLab :: WhiteRef -> XYZ -> LAB
xyzToLab (WhiteRef (wxr, wyr, wzr)) (XYZ (x, y, z)) = LAB (l, a, b)
  where
    (xr, yr, zr) = (x / wxr, y / wyr, z / wzr)
    l = 116 * (pivot yr) - 16
    a = 500 * (pivot xr - pivot yr)
    b = 200 * (pivot yr - pivot zr)
    epsilon = 216 / 24389
    kappa = 24389 / 27
    pivot t = if t > epsilon
        then t ** (1/3)
        else (kappa * t + 16) / 116


-- | Convert 24-bit RGB to LAB using cie profile and white reference
rgbToLab :: CIEProfile -> WhiteRef -> RGB -> LAB
rgbToLab prof white rgb = xyzToLab white $ rgbToXYZWithProfile prof rgb


-- | Convert 24-bit RGB to LAB using sRGB d65 profile
rgbToLabd65 :: RGB -> LAB
rgbToLabd65 = rgbToLab sRGBd65 d65


unLAB :: LAB -> FloatLAB
unLAB (LAB (l, a, b)) = (realToFrac l, realToFrac a, realToFrac b)


xtermColorsLab :: VU.Vector FloatLAB
xtermColorsLab = VU.map (unLAB . rgbToLabd65) xtermColors


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
