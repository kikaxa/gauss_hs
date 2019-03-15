import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Maybe

type Row = UV.Vector Float
type Matrix = V.Vector Row

genRow z n = UV.generate n (\x -> 1 / fromIntegral (1 + z + x) ) `UV.snoc` (1 / fromIntegral (z + 1))
genMatr n = V.generate n (\x -> genRow x n)

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = UV.foldl reduceRow matrix $ UV.enumFromN 0 matrixlength where
 matrixlength = V.length matrix - 1
 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b = let
  (p1, p2) = (V.take a xs, V.drop (a+1) xs)
  (p3, p4) = (V.take (b-a-1) p2, V.drop (b-a) p2)
  in p1 `V.snoc` (xs V.! b) V.++ p3 `V.snoc` (xs V.! a) V.++ p4

 reduceRow matrix1 r = let
  firstnonzero = (+) (r+1) $ fromJust $ V.findIndex (\x -> x UV.! r /= 0) $ V.drop (r+1) matrix1
  matrix2 = if (matrix1 V.! r UV.! r == 0) then (swap matrix1 r firstnonzero) else matrix1
  row = matrix2 V.! r
  row1 = UV.map (\x -> x / (row UV.! r)) row

  subrow nr = UV.zipWith (\a b -> k*a - b) row1 nr
    where k = nr UV.! r
  nextrows = V.map subrow $ V.unsafeDrop (r+1) matrix2
  in V.take r matrix2 `V.snoc` row1 V.++ nextrows

substitute matrix = V.foldr next UV.empty matrix where
 lengthmatrix = V.length matrix
 next row found = let
  subpart = UV.unsafeInit $ UV.unsafeDrop (lengthmatrix - UV.length found) row
  solution = UV.unsafeLast row - UV.sum (UV.zipWith (*) found subpart)
  in solution `UV.cons` found


solve :: Matrix -> Row
solve = substitute . gaussianReduce

main = print . UV.sum . solve $ genMatr 300
