{-# LANGUAGE TemplateHaskell #-}
module System.Console.Xterm.Colors
    ( RGB(..)
    , xtermColors
    ) where


import qualified Data.Vector.Unboxed as VU

import System.Console.Xterm.Types
import qualified System.Console.Xterm.Colors.TH as TH


-- | Unboxed Vectore that holds info about xterm colors
xtermColors :: VU.Vector RGB
xtermColors = $(let val = VU.fromList TH.xtermColors in [| val |])
