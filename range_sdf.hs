module Rangesdf (
  RangeCode, RangeFunc, RgA
  ) where

import ForSyDe.Shallow

RangeCode :: (Maybe Int, Char) -> Char


RangeFunc :: [Maybe (Maybe Int, Char)] -> [Char]
RangeFunc [not (len , sym)]
  | not == Nothing = Nothing
  | otherwise = RangeCode (len , sym)

InitHdr = [dictsize,insize]

RgA :: Signal (Maybe (Maybe Int, Char)) -> Signal Char
RgA Sfc = Scs
  where Scs = actor11SDF 1 1 RangeFunc Sfc'
    Sfc' = delaySDF InitHdr Sfc


