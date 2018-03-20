
import Control.DeepSeq
force x = x `deepseq` x

type Row = [Float]
type Matrix = [Row]

genRow z n = foldr (\x y -> 1 / (z + x) : y) [1 / (z+1)] [1..n]
genMatr n = map (\x -> genRow x n) [1..n]

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = foldl reduceRow matrix [0..length matrix-1] where

 swap xs a b
  | a > b = swap xs b a
  | a == b = xs
  | a < b = let
  (p1,p2) = splitAt a xs
  (p3,p4) = splitAt (b-a-1) (tail p2)
  in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)

 reduceRow matrix1 r = let
  firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix-1]
  matrix2 = if (matrix1 !! r !! r == 0) then (swap matrix1 r firstnonzero) else matrix1
  row = matrix2 !! r
  row1 = map (\x -> x / (row !! r)) row

  subrow nr = zipWith (\a b -> k*a - b) row1 nr
    where k = nr !! r
  nextrows = map subrow $ drop (r+1) matrix2
  in force $ take r matrix2 ++ [row1] ++ nextrows

substitute matrix = foldr next [last (last matrix)] (init matrix) where
 lengthmatrix = length matrix
 next row found = let
  subpart = init $ drop (lengthmatrix - length found) row
  solution = last row - sum (zipWith (*) found subpart)
  in solution : found


solve :: Matrix -> Row
solve = substitute . gaussianReduce

main = print . sum . solve $ force $ genMatr 300
