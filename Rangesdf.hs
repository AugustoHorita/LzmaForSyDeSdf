module Rangesdf (
  rangeFunc, rgA
  ) where

import ForSyDe.Shallow
import Data.Bits
import Data.Char
import Numeric

-- RangeVars = (range, Lower Limit, cache)
type RangeVars = (Int,Int,Char)
rangeInit = 2^32 -1
rangeLimit = 2^24

toBitsBySize :: Int -> Int -> [Bool]
toBitsBySize  0 x = []
toBitsBySize sz 0 = [False | i <- [1..sz]]
toBitsBySize sz x =  if k == 0 
    then False : (toBitsBySize n x) 
    else True  : (toBitsBySize n (x - k*m))
    where n = sz - 1
          m = 2^n
          k = x `div` m

normRange:: [Char] ->[RangeVars] -> ([Char], [RangeVars])
normRange nSout [(nRan,nL,nC)]
  | nRan < rangeLimit = 
    if low < 0xFF000000 then
      ( nC : nSout, [(nRan  * 0x100, low  * 0x100, chr cache)])
    else if nL > 0xFFFFFFFF then
      ((chr (ord nC + 1)) : nSout, [(nRan  * 0x100, low  * 0x100, chr cache)])
    else  
      (nSout,[(nRan  * 0x100,nL,nC)])
  | otherwise = (nSout,[(nRan,nL,nC)])
  where high = quot (nL .&. 0xFFFFFF00000000) 0xFFFFFFFF
        low = (nL .&. 0xFFFFFFFF)
        cache = (quot nL 0x1000000) .&. 0xFF

toSigSig :: ([Char],[RangeVars]) -> ([[Char]],[RangeVars])
toSigSig ([],b) = ([],b)
toSigSig (a,b) = ([a],b)

rangeBits :: [Bool] -> ([Char],[RangeVars]) -> ([Char], [RangeVars]) 
rangeBits [] (a,b) = (a,b)
rangeBits eIn (encSigOut, [(eRan,eL,eC)])
  | head eIn == True =  
    rangeBits (tail eIn) (normRange encSigOut [(newRan,eL + (newRan),eC)])                      
  | head eIn == False =  
    rangeBits (tail eIn) (normRange encSigOut [(newRan,eL,eC)])
  where newRan = quot eRan 2

rangeFunc :: [Maybe (Maybe Int, Char)] -> [RangeVars] -> ([[Char]],[RangeVars])
rangeFunc [Nothing] s_rr = ([[]],s_rr)
rangeFunc [Just (Nothing , sym)] s_rr = toSigSig (rangeBits (toBitsBySize 8 (fromEnum sym)) ([] , s_rr))
rangeFunc [Just (Just len , sym)] s_rr = toSigSig (rangeBits (ranLenChar ++ ranSymChar)  ([] , s_rr))
  where ranLenChar = toBitsBySize 32 len
        ranSymChar = toBitsBySize 8 (fromEnum sym)
-- initHdr = [dictsize,insize]
dictsize = '\x05'

rgA :: Signal (Maybe (Maybe Int, Char)) -> Signal [Char]
rgA sFc = sCs'
  where (sCs,sFb) = actor22SDF (1,1) (1,1) rangeFunc sFc sFb'
        sCs' = delaySDF initHdr sCs
        sFb' = delaySDF [(rangeInit,0, chr 0)] sFb
        initHdr = [([dictsize] ++ [(intToDigit (lengthS sFc))])]



