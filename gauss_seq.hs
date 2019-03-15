
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Maybe
import Control.DeepSeq

mAt = Seq.index
slast x = x `mAt` (Seq.length x - 1)
sinit x = Seq.take (Seq.length x - 1) x

type Row = Seq.Seq Float
type Matrix = Seq.Seq Row

genRow z n = Seq.fromFunction n (\x -> 1 / fromIntegral (z + x)) Seq.|> (1 / fromIntegral (z+1))
genMatr n = Seq.fromFunction n (\x -> genRow x n)

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fst $ foldl reduceRow (Seq.empty :: Matrix, matrix) [0..length matrix-1] where
 lengthmatrix = Seq.length matrix

 reduceRow (res, matrix1) r = let
  matrix2 = if (matrix1 `mAt` 0 `mAt` r == 0) then (swap matrix1 0 firstnonzero) else matrix1
    where firstnonzero = fromJust $ Seq.findIndexL (\x -> x `mAt` r /= 0) matrix1
          swap xs a b = Seq.update a (xs `mAt` b) $ Seq.update b (xs `mAt` a) xs
  row1 = fmap (\x -> x / (row  `mAt`  r)) row
    where row = matrix2 `mAt`  0

  subrow nr = Seq.zipWith (\a b -> k*a - b) row1 nr
    where k = nr  `mAt`  r
  nextrows = fmap subrow $ Seq.drop 1 matrix2
  in (res Seq.|> row1, force $ nextrows)

substitute matrix = foldr next Seq.empty matrix where
 lengthmatrix = Seq.length matrix
 next row found = let
  subpart = sinit $ Seq.drop (lengthmatrix - Seq.length found) row
  solution = slast row - sum (Seq.zipWith (*) found subpart)
  in solution Seq.<| found


solve :: Matrix -> Row
solve = substitute . gaussianReduce

main = print . sum . solve $ genMatr 300
