module Lzmasdf (
    lzmaSdf
  ) where

import ForSyDe.Shallow
import Rangesdf
import LZsdf
import Data.Char

-- Simplified LZMA process network
lzmaSdf :: Signal Char -> Signal [Char]
lzmaSdf sLzmaIs = sLzmaOut
    where sLzmaOut = rgA (lzA sLzmaIs)

inputTest = signal "abracadabra"
lzmaOutTest = mapSY (map ord) $ (lzmaSdf inputTest)