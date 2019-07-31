-- Simplified LZMA process network
lzmaSdf :: Signal Char -> Signal Byte
lzmaSdf Sis = Scs
    where Scs = rgA lzA Sis