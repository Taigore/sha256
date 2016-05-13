module SHA256 where
import SHA256.Operations
import Common
import Words
import Data.Bits
import Data.Word
import Data.List
import Numeric

data PaddedData = Padded [Word8] 
data RawBlock = Raw [Word32]
data BlownBlock = Blown [Word32]
data Hash = Hash Integer
data HashingState = State (Word32,Word32,Word32,Word32,Word32,Word32,Word32,Word32)

instance Show Hash where
  show (Hash x) = showHex x ""

padded :: [Word8] -> PaddedData
padded l = Padded (concat [l, padding]) where
  len = length l
  len_in_bits = len * 8
  len_in_bits_bytes = (unpack . toW64) len_in_bits
  padded_len = head (filter (\x->x`mod`64 == 0) [len+9..])
  padding = 0x80 : (concat [zero_padding, len_in_bits_bytes])
  zero_padding = [0x00 | i <- [1..(padded_len-len-9)]]

blocks :: PaddedData -> [RawBlock]
blocks (Padded l) = map (Raw . pack_list) (cluster_blocks l) where
  pack_list l' = map pack (cluster l' 4)
  cluster_blocks l' = cluster l' 64

c_m :: RawBlock -> BlownBlock
c_m (Raw b) = (Blown b') where
  b' = [w i | i <- [1..64]]
  w i
    | i <= 16 = (b !! (i - 1))
    | otherwise = (lSigma1(w(i-2))) *+ (w(i-7)) *+ (lSigma0(w(i-15))) *+ (w(i-16))

hash :: [BlownBlock] -> Hash
hash b_list = Hash final_state where
  final_state = foldl (*:) 0 [a,b,c,d,e,f,g,h]
  (a,b,c,d,e,f,g,h) = foldl hash_block cHi b_list

hash_block :: W32Tuple8 -> BlownBlock -> W32Tuple8
hash_block s (Blown b) = tuple_add s s' where
  s' = foldl (\a (w,k) -> hash_round a w k) s (zip b cKi)

hash_round :: W32Tuple8 -> Word32 -> Word32 -> W32Tuple8
hash_round (a,b,c,d,e,f,g,h) w k = (a',b',c',d',e',f',g',h') where
  t1 = h *+ (cSigma1 e) *+ (ch e f g) *+ k *+ w
  t2 = (cSigma0 a) *+ (maj a b c)
  a' = t1 *+ t2
  b' = a
  c' = b
  d' = c
  e' = d *+ t1
  f' = e
  g' = f
  h' = g

tuple_add = lift2_tuple8 (*+)

sha256 :: [Word8] -> Hash
sha256 l = (hash . (map c_m) . blocks . padded) l where

