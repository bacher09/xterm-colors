{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.Colors.TH
    ( ansiColors
    , greyScales
    , extraColors
    , xtermColors
    , rgbToLabd65
    , unLAB
    ) where


import Data.Word
import Data.List (unfoldr)
import Control.Applicative


import System.Console.Xterm.Types


ansiColors, greyScales, extraColors, xtermColors :: [RGB]

-- | base ansi colors in xterm: 0-15
ansiColors = [ mkRGB 0x00 0x00 0x00
             , mkRGB 0x80 0x00 0x00
             , mkRGB 0x00 0x80 0x00
             , mkRGB 0x80 0x80 0x00
             , mkRGB 0x00 0x00 0x80
             , mkRGB 0x80 0x00 0x80
             , mkRGB 0x00 0x80 0x80
             , mkRGB 0xc0 0xc0 0xc0
             , mkRGB 0x80 0x80 0x80
             , mkRGB 0xff 0x00 0x00
             , mkRGB 0x00 0xff 0x00
             , mkRGB 0xff 0xff 0x00
             , mkRGB 0x00 0x00 0xff
             , mkRGB 0xff 0x00 0xff
             , mkRGB 0x00 0xff 0xff
             , mkRGB 0xff 0xff 0xff]


-- | additional xterm colors: 16-231 
extraColors = mkRGB <$> combs <*> combs <*> combs
  where
    combs = fromIntegral <$> 0x00 : unfoldr gen 0x5f
    gen :: Int -> Maybe (Int, Int)
    gen r =  if r <= 255 then Just (r, r + 40) else Nothing

-- | xterm grey scales: 232-255
greyScales = (\v -> mkRGB v v v) . fromIntegral <$> vals
  where
    vals :: [Int]
    vals = unfoldr (\r -> if r <= 0xee then Just (r, r + 10) else Nothing) 0x08


xtermColors = ansiColors ++ extraColors ++ greyScales


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
