{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module System.Console.Xterm.Types
    ( RGB(..)
    , XYZ(..)
    , LAB(..)
    , WhiteRef(..)
    , CIEProfile(..)
    , FloatLAB
    , LABDistance
    , mkRGB
    , sRGBd50
    , sRGBd65
    , d65
    , d50
    ) where


import Data.Word
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as VU


-- using float to reduce memory usage
type FloatLAB = (Float, Float, Float)
type LABDistance = Float


newtype RGB = RGB (Word8, Word8, Word8)
    deriving(Show, Eq, Ord)


newtype XYZ = XYZ (Double, Double, Double)
    deriving(Show, Eq, Ord)

newtype LAB = LAB (Double, Double, Double)
    deriving(Show, Eq, Ord)

newtype WhiteRef = WhiteRef (Double, Double, Double)


data CIEProfile = CIEProfile (Double, Double, Double)
                             (Double, Double, Double)
                             (Double, Double, Double)


instance Lift Word8 where
    lift x = [| fromInteger $(lift $ toInteger x) :: Word8 |]


instance Lift Float where
  lift x = [| $(litE $ rationalL $ toRational x) :: Float |]


instance (VU.Unbox a, Lift a) => Lift (VU.Vector a) where
  lift v = [| VU.fromList $(lift $ VU.toList v) |]


instance Lift RGB where
    lift (RGB (r, g, b)) = [| RGB (r, g, b) |]


derivingUnbox "RGB"
    [t| RGB -> (Word8, Word8, Word8) |]
    [| \(RGB (r, g, b)) -> (r, g, b) |]
    [| \(r, g, b) -> RGB (r, g, b) |]


mkRGB :: Word8 -> Word8 -> Word8 -> RGB
mkRGB r g b = RGB $ (r, g, b)


sRGBd65 :: CIEProfile
sRGBd65 = CIEProfile (0.4124564, 0.3575761, 0.1804375)
                     (0.2126729, 0.7151522, 0.0721750)
                     (0.0193339, 0.1191920, 0.9503041)


sRGBd50 :: CIEProfile
sRGBd50 = CIEProfile (0.4360747, 0.3850649, 0.1430804)
                     (0.2225045, 0.7168786, 0.0606169)
                     (0.0139322, 0.0971045, 0.7141733)


d65 :: WhiteRef
d65 = WhiteRef (0.95047, 1.0000001, 1.08883)


d50 :: WhiteRef
d50 = WhiteRef (0.96422, 1.0, 0.82521)
