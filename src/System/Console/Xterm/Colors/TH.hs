{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.Colors.TH
    ( ansiColors
    , greyScales
    , extraColors
    ) where


import Data.List (unfoldr, minimumBy)
import Control.Applicative


import System.Console.Xterm.Types


ansiColors, greyScales, extraColors :: [RGB]

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
