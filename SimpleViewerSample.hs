import Graphics.UI.GLUT
import Data.IORef
import SimpleViewer


v2c :: Vector3 GLfloat -> Color3 GLfloat
v2c (Vector3 a b c) = Color3 a b c

rgb2yc :: Vector3 GLfloat -> Vector3 GLfloat
rgb2yc (Vector3 r g b) = Vector3 cr y cb
  where
    y = 0.299 * r + 0.587 * g + 0.114 * b
    cr = 0.5 * r  - 0.419 * g  - 0.081 * b
    cb = (-0.1699) * r - 0.332 * g + 0.5 * b

main :: IO ()
{-
main = do
  let f1 = (\x y -> x+y)
  let f2 = (\x y -> cos(1/(x*x+y*y+1))-0.5)
  let f3 = (\x y -> log(1/((x-5)**2+(y-5)**2+1))+0.5)
  let f4 = (\x y -> 2.0*exp((-1)*(x*x+y*y)/10)-0.5)
  let f5 = (\x y -> exp (x * y * y /1000))
  viewCreateWindow "Test" $ 
                   [viewRenderFunc  f5 (-5) 11 (-5) 11 32]

  mainLoop
-}
main = do
  let n = 8
  let x i = 1.0/n * i
  let vs = [Vector3 (x i) (x j) (x k)| i<-[0..n], j<-[0..n], k<-[0..n]]
  let rgbs = map (\v -> (v, v2c v)) vs
  let ycs = map (\v -> (rgb2yc v, v2c v)) vs
  let ys  = filter (\(Vector3 y c1 c2, col) -> ((abs c1) < 0.01 && (abs c2) < 0.01)) ycs
  viewCreateWindow "Test" $ 
                   [
                    viewPlotDots3D rgbs, putAxisNames ["R", "G", "B"]
--                     viewPlotDots3D ycs, putAxisNames ["Cr", "Y", "Cb"]
--                   , viewPlotDots3D ys
                   ]
  mainLoop
