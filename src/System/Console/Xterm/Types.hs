module System.Console.Xterm.Types
    ( RGB(..)
    , mkRGB
    ) where


import Data.Word


newtype RGB = RGB (Word8, Word8, Word8)
    deriving(Show, Eq, Ord)


mkRGB :: Word8 -> Word8 -> Word8 -> RGB
mkRGB r g b = RGB $ (r, g, b)
