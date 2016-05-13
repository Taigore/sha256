module Common (lift_tuple2, lift2_tuple8,
               cluster, within,
               Range) where
import Data.List

lift_tuple2 :: (a -> b) -> ((a,a) -> (b,b))
lift_tuple2 f (x,y) = (f x, f y)

cluster :: [a] -> Int -> [[a]]
cluster [] _ = []
cluster l 1 = map (\x -> [x]) l
cluster l n
  | n > 0 = (strip . group . tag) l where
  strip l' = map strip_tags l'
  group l' = groupBy (\x y -> (snd x)==(snd y)) l'
  tag l' = zip l' [i | i <- [1..], x <- [1..n]]
  strip_tags = map fst

lift2_tuple8 :: (a -> b -> c) -> ((a,a,a,a,a,a,a,a) -> (b,b,b,b,b,b,b,b) -> (c,c,c,c,c,c,c,c))
lift2_tuple8 lf (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') = r where
  r = (lf a a',lf b b',lf c c',lf d d',lf e e',lf f f',lf g g',lf h h')

data Range a = Inclusive (a, a)
             | Exclusive (a, a)
             | LInclusive (a, a)
             | RInclusive (a, a)

within :: (Ord a) => a -> Range a -> Bool
within n (Inclusive (l, u)) = (l <= n) && (n <= u)
within n (Exclusive (l, u)) = (l < n) && (n < u)
within n (LInclusive (l, u)) = (l <= n) && (n < u)
within n (RInclusive (l, u)) = (l < n) && (n <= u)

