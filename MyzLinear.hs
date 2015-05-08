--
-- Gaussの消去法
--   水野 (of.mizno@gmail.com)
--   2013/10/17
--   2013/10/19
--      検証用の行列生成関数を追加
--   2013/10/30
--      powerMethod を不動点を探す方式に変更
--   2013/11/12
--      モジュール名を MyzLinearに変更
--


--
-- usage:
--  ghc -Wall -c MyzLinear.hs
--

module MyzLinear where

import Data.Array
import Data.List
import Debug.Trace()

import System.Random
import Control.Monad(forM)
import Test.QuickCheck

import Prelude hiding (id, pi)

type NumType = Double

epsilon :: NumType
epsilon = 1.0e-7

near :: NumType -> NumType -> Bool
near x y = (abs (x - y)) < epsilon

isZero :: NumType -> Bool
isZero x = near x 0

type Matrix = Array (Int, Int) NumType
type P = [(Int, Int)]
type Pivots = [(Int, Int)]

newMatrix :: Int -> Int -> Int -> Int -> [NumType] -> Matrix
newMatrix iS iE jS jE vs = listArray ( (iS, jS), (iE, jE) ) vs

newMatrix' :: Int -> Int -> Int -> Int -> [((Int,Int), NumType)] -> Matrix
newMatrix' iS iE jS jE vs = array ( (iS, jS), (iE, jE)) vs 

iStart, iEnd, jStart, jEnd :: Matrix -> Int
iStart m = fst.fst $ bounds m
iEnd m = fst.snd $ bounds m
jStart m = snd.fst $ bounds m
jEnd m = snd.snd $ bounds m

rowIndices, colIndices :: Matrix -> [Int]
rowIndices m = [(iStart m) .. (iEnd m)]
colIndices m = [(jStart m) .. (jEnd m)]
-- indices m is defined in Array

rowSize, colSize :: Matrix -> Int
rowSize m = (iEnd m) - (iStart m) + 1
colSize m = (jEnd m) - (jStart m) + 1

idInRange :: Matrix -> Int -> Int -> Bool
idInRange m i j = inRange ( bounds m) (i, j)

stdout :: Matrix  -> IO()
stdout m = putStr $ matrix2str m

matrix2str :: Matrix -> String
matrix2str a = itemLine ++ (concat $ map (\i -> (row2str i)) $ rowIndices a)
  where
    itemLine = "    [" ++ (concat $ map (\j -> "  " ++ (show j)) (colIndices a)) ++ "]\n"
    row2str i = (show i) ++ ": " ++ (concat $ map (\j -> ("  "++(show (a!(i,j))))) (colIndices a)) ++ "\n"

ll2str :: (Show a) => [[a]] -> String
ll2str [] = ""
ll2str (l:ls) = (list2str l) ++ "\n" ++ (ll2str ls)

list2str :: (Show a) => [a] -> String
list2str [] = ""
list2str (l:ls) = "  " ++ (show l) ++ (list2str ls)

sumOfRow, sumOfColumn :: Matrix -> Int -> NumType
sumOfRow m i = sum $ map (\j -> m!(i,j)) (colIndices m)
sumOfColumn m j = sum $ map (\i -> m!(i,j)) (rowIndices m)

averageRow, averageColumn :: Matrix -> Matrix
averageRow m = newMatrix 0 0 (jStart m) (jEnd m) $ map (\j -> (sumOfColumn m j)/(fromIntegral (rowSize m)) ) (colIndices m)
averageColumn m = newMatrix (iStart m) (iEnd m) 0 0 $ map (\i -> (sumOfRow m i)/(fromIntegral (colSize m))) (rowIndices m)

geomAverageColumn, geomAverageRow :: Matrix -> Matrix
geomAverageColumn m = newMatrix (iStart m) (iEnd m) 0 0  [(productOfRow m i) ** (1.0 / (fromIntegral $ colSize m)) | i <- (rowIndices m)]
geomAverageRow m = newMatrix 0 0 (jStart m) (jEnd m) [(productOfColumn m j) ** (1.0 / (fromIntegral $ rowSize m)) | j <- (colIndices m)]

productOfRow :: Matrix -> Int -> NumType
productOfRow m i = product [(m!(i,j)) | j <- (colIndices m)]

productOfColumn :: Matrix -> Int -> NumType
productOfColumn m j = product [m!(i,j) | i <- (rowIndices m)]

trans :: Matrix -> Matrix
trans m = newMatrix' (jStart m) (jEnd m) (iStart m) (iEnd m) [((j,i), m!(i,j)) | (i,j) <- (indices m)]

multi :: Matrix -> Matrix -> Matrix
multi m1 m2 = newMatrix' (iStart m1) (iEnd m1) (jStart m2) (jEnd m2) [((i,j), f i j) | i <- (rowIndices m1), j <- (colIndices m2)]
  where
    f i j = sum $ map (\k -> (safeAt m1 i k) * (safeAt m2 k j)) $ colIndices m1
    safeAt m i j
      | idInRange m i j = m!(i,j)
      | otherwise = 0

multiScalar :: NumType -> Matrix -> Matrix
multiScalar c m = array (bounds m) [(id, c * m!id) | id <- indices m]

add :: Matrix -> Matrix -> Matrix
add a b = newMatrix' (iStart a) (iEnd a) (jStart a) (jEnd a) [((i, j), a!(i,j) + b!(i,j))|(i,j) <- (indices a)]

sub :: Matrix -> Matrix -> Matrix
sub a b = newMatrix' (iStart a) (iEnd a) (jStart a) (jEnd a) [((i, j), a!(i,j) - b!(i,j))|(i,j) <- (indices a)]


addAbs :: Matrix -> Matrix -> Matrix
addAbs a b = newMatrix' (iStart a) (iEnd a) (jStart a) (jEnd a) [((i, j), (abs $ a!(i,j)) + (abs $ b!(i,j)))|(i,j) <- (indices a)]

innerProduct :: Matrix -> Matrix -> NumType
innerProduct x y = (multi (trans x) y) ! (jStart x, jStart y)

normalizeColumn :: Matrix -> Int -> Matrix
normalizeColumn m k = array (bounds m) [ ((i,j), f i j) | (i,j) <- (indices m)]
  where
     deno = sumOfColumn m k
     f i j
       | k == j = m!(i,j)/deno
       | otherwise = m!(i,j)

-- Euclidian norm
normalizeColumn2 :: Matrix -> Int -> Matrix
normalizeColumn2 m k = array (bounds m) [ ((i,j), f i j) | (i,j) <- (indices m)]
  where
     deno = norm2 m k
     f i j
       | k == j = m!(i,j)/deno
       | otherwise = m!(i,j)

norm2 :: Matrix -> Int -> NumType
norm2 mat j = sqrt $ sum [ mat!(i,j)^(2::Integer) | i <- (rowIndices mat)]


-- for permnuation
newP :: Matrix -> P
newP m = zip ( rowIndices m) (rowIndices m)

compileP :: P -> P
compileP [] = []
compileP ( (i,j): ps )
  | i == j    =  compileP ps
  | otherwise =  (i,j) : ( compileP ps )

val :: Int -> [(Int, Int)] -> Int
val i ijs = 
       case lookup i ijs  of
        Just j   -> j
        Nothing  -> i
--   If  Nothing, I want to arise runtime error.

key :: Int -> [(Int, Int)] -> Int
key _ [] = (-10000000000000) -- fail, I want to asize runtime error.
key j (ij:(ijs))
  | j == (snd ij)  =  (fst ij)
  | otherwise  = key j ijs

swapP :: P -> Int -> Int -> P
swapP ps i k = swapP' ps i k (val i ps)  (val k ps)

swapP' :: P -> Int -> Int -> Int ->Int -> P
swapP' [] _ _ _ _ = []
swapP' (p:ps) i k oldi oldk
  | i == (fst p)  =  (i, oldk) : (swapP' ps i k oldi oldk)
  | k == (fst p)  =  (k, oldi) : (swapP' ps i k oldi oldk)
  | otherwise     =   p :  (swapP' ps i k oldi oldk)


p2i :: P -> Int -> Int
p2i p i = val i p


-- for Gaussian elimination

absMaxI :: [(Int, NumType)] -> Int
absMaxI [] = (-1000000000)  -- error
absMaxI ((ai,av):aivs) = maxI' ai av aivs
  where
    maxI' i _ [] = i
    maxI' i v ((j,u): ivs) 
      | (abs v) > (abs u)     =  maxI' i v ivs
      | otherwise =  maxI' j u ivs

searchMaxI :: P -> Matrix -> Int -> Int -> Int
searchMaxI p m i j = absMaxI $ map (\k -> (k, m!(p2i p k, j))) [i .. (iEnd m)]


updateP :: P -> Matrix -> Int -> Int -> P
updateP p m pi pj = swapP p pi (searchMaxI p m pi pj)

underColumnsAreZero :: P -> Matrix -> Int -> Int -> Bool
underColumnsAreZero p m pi pj = and $ map (\i -> (isZero (m!(p2i p i, pj)) )) [pi .. (iEnd m)]

nextPivotj :: P -> Matrix -> Int -> Int -> Maybe (P,Int)
nextPivotj oldp m pi j
  | not (idInRange m pi j) = Nothing
  | otherwise =
      if  underColumnsAreZero p m pi j
      then  nextPivotj p m pi (j + 1)
      else Just (p, j)
  where
    p = updateP oldp m pi j        

forwardOneLineA :: P -> Matrix -> Matrix -> Int -> Int -> Int -> [Int] -> [((Int, Int), NumType)]
forwardOneLineA p a _ pi pj i js = [( (pci,j), (fai j) )| j <- js]
  where
    pci  = p2i p i
    pvi = p2i p pi
    pvv  = a!(pci, pj) / a!(pvi, pj)
    fai j 
      |  ( i  >  pi) && (j > pj)   = a!(pci, j) - (pvv  * a!(pvi, j))
      |  ( i  >  pi) && (j == pj)  = pvv
      |  otherwise  = a ! (pci,j)

forwardOneLineB :: P -> Matrix -> Matrix -> Int -> Int -> Int -> [Int] -> ((Int, Int), NumType)
forwardOneLineB p a b pi pj i _ = ((pci, 0), (fbi))
  where
    pci  = p2i p i
    pvi = p2i p pi
    pvv  = a!(pci, pj) / a!(pvi, pj)
    fbi
      |  (i >  pi)    =  b!(pci, 0) - (pvv * b!(pvi, 0))
      |  otherwise =  b!(pci,0)


forwardRows :: P -> Matrix -> Matrix -> Int -> Int -> (Matrix, Matrix)
forwardRows p a b pi pj = 
     (array (bounds a) $ concat $ [forwardOneLineA p a b pi pj i js | i <- (rowIndices a)]
      ,
      array (bounds b) [forwardOneLineB p a b pi pj i js | i <- (rowIndices a)])
  where
     js = colIndices a

forward :: P -> Matrix -> Matrix -> Int -> Int -> Pivots -> (P, Matrix, Matrix, Pivots)
forward oldp a b pi j pivots
  | not (idInRange a pi j) = (oldp, a, b, pivots)
  | otherwise   =
     case  nextPivotj oldp a pi j of
       Nothing  -> (oldp, a, b, pivots)
       Just (p, pj)  ->  forward p newA newb (pi + 1) (pj + 1) ((pi,pj):pivots)
         where
           (newA, newb) = forwardRows p a b pi pj


baseColIndices, baseRowIndices :: Pivots -> [Int]
baseColIndices pivots = map (\p -> (snd  p)) pivots
baseRowIndices pivots = map (\p -> (fst  p)) pivots

freeColIndices, freeRowIndices :: Matrix -> Pivots -> [Int]
freeColIndices m pivots = (colIndices m) \\ (baseColIndices pivots)
freeRowIndices m pivots = (rowIndices m) \\ (baseRowIndices pivots)

backwardOneLine :: P -> Matrix -> Matrix -> Matrix -> Matrix -> Pivots -> Int -> [Int] -> [Int] -> NumType
backwardOneLine p a b prex x pivots j freeColIds kIds
  | elem j freeColIds = prex!(j, 0)
  | otherwise = (b!(pivotj2i, 0) - (sum $ map (\k -> a!(pivotj2i, k) * (x!(k,0))) kIds )) / a!(pivotj2i, j)
    where
      pivotj2i = p2i p $ key j pivots

backward :: P -> Matrix -> Matrix -> Matrix -> Pivots -> Matrix
backward p a b prex pivots = x 
 where
   x = array (bounds prex) [((j,0), 
                              backwardOneLine p a b prex x pivots j (freeColIndices a pivots) [(j+1) .. (jEnd a)]
                             ) | j <- (reverse $ colIndices a)]

solvable :: P -> Matrix -> Matrix -> Pivots -> Bool
solvable p a b pivots = and $ map (\i -> isZero (b!(p2i p i, 0))) $ freeRowIndices a pivots

gauss :: Matrix -> Matrix -> (Bool, P, Matrix, Matrix, Pivots, Matrix, [Matrix])
gauss a b = 
  if (solvable p u ub pivots)
  then (True, p,u,ub, pivots, x, xs)
  else (False, p,u,ub, pivots, x, [])
    where
      (usedp, u, ub, pivots) =  forward (newP a) a b (iStart a) (jStart a) []
      p = compileP usedp
      x = backward p u ub (makex 0 0) pivots
      xs = map (\j -> backward p u (makeb 0 0) (makex j 1) pivots) $ freeColIndices a pivots
      makex j v = newMatrix' (jStart a) (jEnd a) 0 0 [((k,0), f j k v) | k <- (colIndices a)]
      makeb i v = array (bounds b) [( (k,0), f i k v) | k <- (rowIndices a)]
      f j k v
        | j == k    = v
        | otherwise = 0


--
-- rank of matrix
--
rank :: Matrix -> Int
rank a = length pivots
     where
        (_, _, _, _, pivots, _, _) = gauss a $ averageColumn a

--
-- least square method by gaussian elimination
--
leastSquare :: Matrix -> Matrix -> (Bool, P, Matrix, Matrix, Pivots, Matrix, [Matrix])
leastSquare a b = gauss (multi (trans a) a) (multi (trans a) b)

--
-- power method
--

matDiff :: Matrix -> Matrix -> NumType
matDiff a b = (sum [abs $ a!(i,j) - b!(i,j) | (i,j) <- (indices a)]) / (fromIntegral (rowSize a)) / (fromIntegral (colSize a))

matDiff2 :: Matrix -> Matrix -> NumType
matDiff2 a b = sqrt (sum [(a!(i,j) - b!(i,j))^(2::Integer) | (i,j) <- (indices a)])

searchFixedMatrix :: (Matrix -> Matrix) -> Matrix -> Int -> (Int, Matrix, NumType)
searchFixedMatrix f inix itrMax = sfv inix 0
  where
     sfv x itr
        | itr > itrMax = (itr, x, d)
        | isZero d     = (itr, fx, d)
        | otherwise    = sfv fx (itr + 1)
       where
         fx = f x
         d = matDiff x fx

searchFixedMatrixE :: (Matrix -> (Matrix, NumType)) -> (Matrix, NumType) -> Int -> (Int, Matrix, NumType)
searchFixedMatrixE f (inix, inie) itrMax = sfv inix inie 0
  where
     sfv x eOld itr
        | itr > itrMax = (itr, x, d)
        | isZero d     = (itr, fx, d)
        | e > eOld     = (itr, x, d)
        | otherwise    = sfv fx e (itr + 1)
       where
         (fx, e) = f x
         d = matDiff x fx

powerMethod :: Matrix -> Int -> (Int, NumType, Matrix, NumType)
powerMethod a itrMax = (itr, l, x', d)
  where
    (itr, x', d) = searchFixedMatrix (\x -> normalizeColumn (multi a x) 0) (averageColumn a) itrMax
    lx' = multi a x'
    l = (sumOfColumn lx' 0) / (sumOfColumn x' 0)

filledMat :: Matrix -> NumType -> Matrix
filledMat a v = array (bounds a) [ ((i,j), v) | (i,j) <- (indices a)]


--
-- samples --
--
matA20, b20 :: Matrix
matA20 = newMatrix (-1) 1 (-2) 0 [2, 1, 1,     4, 1, 0,    -2, 2, 1]
b20 = newMatrix (-1) 1 0 0 [1, -2, 7]

matA58, b58, b58b, b58t :: Matrix
matA58 = newMatrix (-1) 1 (-2) 1 [       
  1, 3, 3, 2,
  2, 6, 9, 5,
  -1, -3, 3, 0
   ]
b58 = newMatrix (-1) 1 0 0 [1, 2, -1]
b58b   = newMatrix (-1) 1 0 0 [1, 2, -1.5]
b58t = newMatrix (-2) 1 0 0 [1, 2, -1,3]

matA132, b132 :: Matrix
matA132 = newMatrix (-2) 1 3 4 [1,0,   1,1,   1,3,   1,4]
b132 = newMatrix (-2) 1 0 0 [0,1,2,5]

matP :: Matrix
matP = newMatrix 1 6 1 6 [
   87,  270, -12,  -49, -276,  40,
   -14, -45,   6,  10,   46,  -4,
   -50, -156,  4,  25,  162, -25,
   94,   294, -5,  -47, -306, 49,
   1,    1,  3,   1,   0,   2,
   16,   48,  1,  -6,  -48,  8
   ]

unitMatrix :: Int -> Int -> Int -> Int -> Matrix
unitMatrix is ie js je = newMatrix' is ie js je [((i,j), f i j) | i <- [is..ie], j <- [js..je]]
  where
    f i j
      |  i == j  = 1.0
      |  otherwise = 0.0

--- for test ----

-- ghc -O2 -prof -auto-all source.hs -rtsopts
-- exefile 20 20 +RTS -p

colDiff :: Matrix -> Int -> Matrix -> Int -> NumType
colDiff x j x' j' = (sum $ map (\i -> abs (x!(i,j) - x'!(i,j'))) (rowIndices x))/ (fromIntegral $ rowSize x)

multiXs :: Matrix -> [Matrix] -> Matrix
multiXs a [] = newMatrix (iStart a) (iEnd a) 0 0 [0, 0 .. ]
multiXs a (x:xs) =  addAbs (multi a x) $ multiXs a xs

tolerantSize :: Int
tolerantSize = 200

isSufficientZero:: NumType -> Bool
isSufficientZero x = (abs x) < 10e-5 

seed_check :: Int -> IO Bool
seed_check seed = do
  let ns = (randoms (mkStdGen seed)) :: [Int]
  let (_:_:prem:pren:_) = take 4 ns
  let m = 1 + ( rem (abs prem) tolerantSize)
  let n = 1 + ( rem (abs pren) tolerantSize)
  seed_check_mn seed m n

seed_check_mn :: Int -> Int -> Int -> IO Bool
seed_check_mn seed m n = do
  let ns = (randoms (mkStdGen seed)) :: [Int]
  let rs = (randomRs (-1000, 1000)(mkStdGen seed)) :: [NumType]
  let (iS:jS:_:_:_) = take 4 ns
  let vals = take ((m * n) + n) rs
  let a = newMatrix iS (iS + m -1) jS (jS + n -1) $ take (m * n) vals
  let realx = newMatrix jS (jS + n - 1) 0 0 $ drop (m * n) vals
  let b = multi a realx
  let (flg, p,u,ub, pivots, x, xs) = gauss a b
  let ax = multi a x
  let dist = colDiff b 0 ax 0
  let distXs = (sumOfColumn (multiXs a xs) 0)
  let freeDim = n - (length pivots)
  print "(seed, (iStart, jStart), (M, N), freeDim, dist, distXs) ="
  print (seed, (iS, jS), (m, n), freeDim, dist, distXs)
  if  flg && (isSufficientZero $ dist + distXs)
  then 
    return True
  else do
    stdoutSystem flg p a b u ub pivots x xs
    print "dist, distXs"
    print $ (dist, distXs)
    print "seed"
    print seed
    print "(solvavle?, seed, (iStart, jStart), (M, N), dist, distXs) ="
    print (flg, seed, (iS, jS), (m, n), dist, distXs)
    return False

itr_seed_check :: Int -> Int -> IO()
itr_seed_check seed itrMax = itr_seed_check' seed 0
   where
     itr_seed_check' :: Int -> Int -> IO()
     itr_seed_check' s itr = do
            flgOK <- seed_check s
            if (itr < itrMax) && flgOK then itr_seed_check' (s + 1) (itr + 1) else print "end"


stdoutSystem :: Bool -> P -> Matrix -> Matrix -> Matrix -> Matrix -> Pivots -> Matrix -> [Matrix] -> IO()
stdoutSystem flgSolvable p a b u ub pivots x xs = do
  print "Solvable?"
  print flgSolvable
  print "P="
  print p
  print "Pivots="
  print pivots
  print "A="
  stdout a
  print "b="
  stdout b
  print "U="
  stdout u
  print "ub="
  stdout ub
  print "x(calculated)="
  stdout x
  print "null space dimenssion = "
  print "xs"
  _ <- forM xs (\mat -> stdout mat) 
  print $ (colSize a) - (length pivots)

--- for QuickCheck ---
-- quickCheck  test_gauss

test_gauss :: Int -> Int -> Int -> Int -> Gen Bool
test_gauss iS jS prem pren = do
  let m = 1 + (mod (abs prem) tolerantSize)
  let n = 1 + (mod (abs pren) tolerantSize)
  aVals <- (vector (m * n) :: Gen [NumType])
  xVals <- (vector n :: Gen [NumType])
  let a = newMatrix iS (iS + m - 1) jS (jS + n - 1) aVals
  let realx = newMatrix jS (jS + n - 1) 0 0 xVals
  let b = multi a realx
  let (_, _, _, _, _, x, xs) = gauss a b
  let ax = multi a x
  return  $ isSufficientZero $ (colDiff ax 0 b 0) + (sumOfColumn (multiXs a xs) 0)
