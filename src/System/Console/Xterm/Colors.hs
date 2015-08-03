{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.Colors
    ( RGB(..)
    , xtermColors
    ) where


import qualified Data.Vector.Unboxed as VU

import System.Console.Xterm.Types
import qualified System.Console.Xterm.Colors.TH as TH


xtermColors :: [RGB]
xtermColors = $(let x = TH.xtermColors in [| x |])
