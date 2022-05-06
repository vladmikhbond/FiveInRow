-- https://ru.wikipedia.org/wiki/%D0%A3%D0%BF%D1%80%D0%B0%D0%B2%D0%BB%D1%8F%D1%8E%D1%89%D0%B8%D0%B5_%D0%BF%D0%BE%D1%81%D0%BB%D0%B5%D0%B4%D0%BE%D0%B2%D0%B0%D1%82%D0%B5%D0%BB%D1%8C%D0%BD%D0%BE%D1%81%D1%82%D0%B8_ANSI#Windows_%D0%B8_DOS
module Consul where

clrscr = "\27[0;0H\27[J"
origin =  "\27[0;0H"
hideCur = "\27[?25l" -- CSI ?25l
showCur = "\27[?25h" -- CSI ?25h

-- text colors
red = "\27[31m"
green = "\27[32m"
yellow = "\27[33m"
blue = "\27[34m"
magenta = "\27[35m"
cian = "\27[36m"
norm = "\27[m"
gray = "\27[1m\27[30m"

-- cur movement
rc row col = "\27["++ show row ++";"++ show col ++"f"  -- row,col = 1,2,3...
up n = "\27["++ show n ++"A"
down n = "\27["++ show n ++"B"
forward n = "\27["++ show n ++"C"
back n = "\27["++ show n ++"D"



