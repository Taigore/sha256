module Words(toW64, toW32, toW8, pack, unpack) where
import Data.Bits
import Data.Word

toW8 :: (Integral a, Bits a) => a -> Word8
toW8 = fromIntegral . (0xFF .&. )

toW32 :: (Integral a, Bits a) => a -> Word32
toW32 = fromIntegral . (0xFFFFFFFF .&. )

toW64 :: (Integral a, Bits a) => a -> Word64
toW64 = fromIntegral . (0xFFFFFFFFFFFFFFFF .&. )

class Splittable a where
  unpack :: a -> [Word8]

instance Splittable Word64 where
  unpack w = map (toW8 . (\x -> w`shiftR`(x*8))) [7,6..0]

instance Splittable Word32 where
  unpack w = map (toW8 . (\x -> w`shiftR`(x*8))) [3,2,1,0]

class Packable a where
  pack :: [Word8] -> a

instance Packable Word32 where
  pack [a,b,c,d] = foldl (\a x -> (a`shiftL`8) .|. x) 0 (map toW32 [a,b,c,d])

instance Packable Word64 where
  pack [a,b,c,d,e,f,g,h] = foldl (\a x -> (a`shiftL`8) .|. x) 0 (map toW64 [a,b,c,d,e,f,g,h])
