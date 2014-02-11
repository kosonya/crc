import Data.Word
import Data.Bits

showBits::Word8 -> String
showBits byte = map (\x -> if (testBit byte x) then '1' else '0') [7,6..0]

encode::Word8 -> Word8 -> Word8
encode inf pol = a +  (a `mod` pol) where a = inf `shift` 4

readBits' ('0':s) x = readBits' s (x * 2) 
readBits' ('1':s) x = readBits' s (x * 2 + 1)
readBits' [] x = x
readBits s = readBits' s 0

calcones byte = foldl (\x y -> x + if y == '1' then 1 else 0) 0 (showBits byte)


decode::Word8 -> Word8 -> Word8
decode'::Int -> Word8 -> Word8 -> Word8
decode' 8 _ _ = error "OLOLOLOLOLOLO"
decode' sht code pol = if (calcones res) <= 1 then (code `xor` res) `rotate` (negate sht) else decode' (sht+1) ((code `rotate` 1)::Word8) pol where res = code `mod` pol
decode = decode' 0

-- | / | /""  /""  V
-- |/  | \__  \__  |
-- |\  |    \    \
-- | \ |  __/  __/ *
