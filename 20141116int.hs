import Graphics.UI.GLUT
import Data.IORef
import SimpleViewer
import Data.Array
import Debug.Trace

import MyzLinear


{-
f :: Double -> Double
f x = 1.0/(1.0 + 25.0 * x^2)
-}

{-
f :: Double -> Double
f x = (sin x)^2 *(cos x)+(1/3/(exp x))
-}


f :: Double -> Double
f x = sin x


{-
f::Double -> Double
f x = (x ^ 2 + 1)/(x^3 + x)
-}

makeXs :: Int -> [Double]
makeXs n = map (\i -> x0 + (fromIntegral i)*((xn-x0)/((fromIntegral (n-1))))) [0..(n-1)]
  where
    x0 = (-1.2)
    xn = 1.2

xs3 = makeXs 3
xs5 = makeXs 5
xs9 = makeXs 9
xsample = [(-1.1), (-0.5), (-0.2), 0.1, 0.3, 0.7, 0.8, 1.0]

ys3 = map f xs3
ys5 = map f xs5
ys9 = map f xs9
ysample = map f xsample

pcof :: [Double] -> [Double] -> MyzLinear.Matrix
pcof axs ays = cof
   where
      (_, _,_,_, _, cof, _) = gauss matA b
      matA = MyzLinear.newMatrix' 0 ((length axs)-1) 0 ((length axs)-1) [((i,j), (axs!!i)^j) | i <- [0..((length axs)-1)], j <- [0..((length axs)-1)]]
      b = MyzLinear.newMatrix 0 (length axs) 0 0 ays

pf :: MyzLinear.Matrix -> Double -> Double
pf acof x = sum $ map (\i -> acof!(i,0) * x^i) $ rowIndices acof


newSpline :: [Double] -> [Double] -> MyzLinear.Matrix
{-newSpline axs ays = newMatrix' 1 ((length axs) - 1) 2 ((length axs) -1) 
                     [((i, 2), 0)]
-}
newSpline axs ays = cof
    where
      cof = newMatrix' 0 ((length axs)-1) 0 3
                     $ [((i,0), ays!!i) | i <- [0..((length axs) -1)]]
                       ++ [((i,1), f1 i) | i <- [0..((length axs) -1)]]
                       ++ [((i,2), f2 i) | i <- [0..((length axs) -1)]]
                       ++ [((i,3), axs!!i) | i <- [0..((length axs) -1)]]
      f2 i
        | i == 0 = 0.0
        | i == ((length axs)-1) = 0.0
        | otherwise = y2!(i,0)
      f1 i
        | i == ((length axs) -1) = 0.0
--        | i == ((length axs) -2) = 0.0
--        | otherwise = 0.0
        | otherwise = (ays!!(i+1) - ays!!i)/(axs!!(i+1) - axs!!i) - cof!(i+1,2)*(axs!!(i+1)-axs!!i)/6.0 - cof!(i,2) * (axs!!(i+1) - axs!!i)/3.0
      matH = newMatrix' 1 ((length axs)-2) 1 ((length axs) -2)
              [((i, j), fh i j) | i <- [1..((length axs)-2)], j<-[1..((length axs)-2)]]
      fh i j
        | i == j  = 2.0 * ((axs!!(i)-axs!!(i-1))  + (axs!!(i+1)-axs!!(i)))
        | j == (i+1) = axs!!(j+1) - axs!!j
        | j == (i-1) = axs!!(i+1) - axs!!i
        | otherwise = 0.0
      gamma = newMatrix' 1 ((length axs)-2) 0 0
               [((i,0), 6.0*((ays!!(i+1) - ays!!i)/(axs!!(i+1) - axs!!i) - (ays!!i - ays!!(i-1))/(axs!!i - axs!!(i-1))))| i<-[1..((length axs)-2)]]
      (_, _,_,_, _, y2, _) =  gauss matH gamma

interval :: MyzLinear.Matrix -> Double -> Int
interval cof x
  | x <= cof!(0,3) = 0
  | x >= cof!(iEnd cof, 3) = (iEnd cof) -1
  | otherwise = int' cof x (iStart cof)
     where
       int' cof x i
        | i >= (iEnd cof) = (iEnd cof) -1 
        | otherwise = if (cof!(i,3) <= x) && (x <= cof!(i+1,3)) then i else int' cof x (i+1)

spline :: MyzLinear.Matrix -> Double -> Double
spline c x = c!(i,0) 
             + c!(i,1)*(x-c!(i,3)) 
             + c!(i,2)*(x-c!(i,3))**2.0/2.0
             + (c!(i+1,2)-c!(i,2))*(x-c!(i,3))**3.0/(6.0*(c!(i+1,3)-c!(i,3)))
   where
     i = interval c x

spline1 :: MyzLinear.Matrix -> Double -> Double
spline1 c x = c!(i,1) 
             + c!(i,2)*(x-c!(i,3))
             + (c!(i+1,2)-c!(i,2))*(x-c!(i,3))**2.0/(2.0*(c!(i+1,3)-c!(i,3)))
   where
     i = interval c x

spline2 :: MyzLinear.Matrix -> Double -> Double
spline2 c x = c!(i,2) 
             + (c!(i+1,2)-c!(i,2))*(x-c!(i,3))/(c!(i+1,3)-c!(i,3))
   where
     i = interval c x

spline3 :: MyzLinear.Matrix -> Double -> Double
spline3 c x =  (c!(i+1,2)-c!(i,2))/(c!(i+1,3)-c!(i,3))
   where
     i = interval c x

main = do
  let cof3 = pcof xs3 ys3
  let cof5 = pcof xs5 ys5
  let cof9 = pcof xs9 ys9
  let cofs = pcof xsample ysample
  let s = newSpline xs9 ys9
  let xmin = (minimum (map realToFrac xs9)) * 1.05
  let xmax = (maximum (map realToFrac xs9)) * 1.05  
  viewCreateWindow "Test" $ 
                   [
                    putAxisNames ["x", "", ""]
--                  , viewRenderFunc2D  (realToFrac.f.realToFrac) (Color3 0 1 0) xmin xmax 10--0
--                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.9)) xs9, viewRenderFunc2D  (realToFrac.(pf cof9).realToFrac) (Color3 1 0 0.9) xmin xmax 100
--                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.5)) xs5, viewRenderFunc2D  (realToFrac.(pf cof5).realToFrac) (Color3 1 0 0.5) xmin xmax 100
--                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xs3 , viewRenderFunc2D  (realToFrac.(pf cof3).realToFrac) (Color3 1 0 0.3) xmin xmax 100
--                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xsample                                  
--                  , viewRenderFunc2D  (realToFrac.(pf cofs).realToFrac) (Color3 1 0 0.3) xmin xmax 100
--                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xsample , viewRenderFunc2D  (realToFrac.(spline s).realToFrac) (Color3 1 0 0.3) xmin xmax 100
                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xs9 , viewRenderFunc2D  (realToFrac.(spline s).realToFrac) (Color3 1 0 0.3) xmin xmax 100
                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xs9 , viewRenderFunc2D  (realToFrac.(spline1 s).realToFrac) (Color3 1 0 0.3) xmin xmax 100
                  , viewPlotDots2D $ map (\x -> (Vector2 (realToFrac x) (realToFrac  (f (realToFrac x))), Color3 1 0 0.3)) xs9 , viewRenderFunc2D  (realToFrac.(spline2 s).realToFrac) (Color3 1 0 0.3) xmin xmax 100
                   ]
  mainLoop
