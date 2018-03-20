import Data.Array.Unboxed
import Data.Array.IArray


type Row = UArray Int Float
type Matrix = Array Int Row

genRow z n = foldr (\x y -> 1 / fromIntegral (z + x) : y) [1 / fromIntegral (z + 1)] [1..n]
genMatr n = listArray (0, n-1) $ map (\x -> listArray (0, n) (genRow x n)) [1..n]

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = foldl reduceRow matrix [0..matrixlength - 1] where
 matrixlength = length $ elems matrix
 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b = let
  (p1,p2) = splitAt a xs
  (p3,p4) = splitAt (b-a-1) (tail p2)
  in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

 reduceRow matrix1 r = let
  firstnonzero = head $ filter (\x -> matrix1 ! x ! r /= 0) [r..matrixlength-1]
  matrix2 = if (matrix1 ! r ! r == 0) then (listArray (0, matrixlength-1) (swap (elems matrix1) r firstnonzero)) else matrix1
  row = matrix2 ! r
  row1 = amap (\x -> x / (row ! r)) row
  row1list = elems row1
  subrow nr = accum (\x a -> k*a - x) nr $ zip [0..matrixlength] row1list
    where k = nr ! r
  nextrows = drop (r+1) $ elems $ amap subrow matrix2
  in (//) matrix2 $ zip [r..matrixlength-1] $ row1 : nextrows

substitute matrix = foldr next [last (elems (last matrix))] (init matrix) where
 matrixlength = length matrix
 next row found = let
  subpart = init $ drop (matrixlength - length found) rowlist
  solution = last rowlist - sum (zipWith (*) found subpart)
  in solution : found 
    where rowlist = elems row

solve :: Matrix -> [Float]
solve = substitute . elems . gaussianReduce

main = print . sum . solve $ genMatr 300

