{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module System.Console.Xterm.Types
    ( RGB(..)
    , mkRGB
    ) where


import Data.Word
import Language.Haskell.TH.Syntax
import Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Unboxed as VU


newtype RGB = RGB (Word8, Word8, Word8)
    deriving(Show, Eq, Ord)


instance Lift Word8 where
    lift x = [| fromInteger $(lift $ toInteger x) :: Word8 |]


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
