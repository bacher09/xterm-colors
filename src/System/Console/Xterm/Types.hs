{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}
module System.Console.Xterm.Types
    ( RGB(..)
    , mkRGB
    ) where


import Data.Word
import Data.Vector.Unboxed.Deriving


newtype RGB = RGB (Word8, Word8, Word8)
    deriving(Show, Eq, Ord)


derivingUnbox "RGB"
    [t| RGB -> (Word8, Word8, Word8) |]
    [| \(RGB (r, g, b)) -> (r, g, b) |]
    [| \(r, g, b) -> RGB (r, g, b) |]


mkRGB :: Word8 -> Word8 -> Word8 -> RGB
mkRGB r g b = RGB $ (r, g, b)
