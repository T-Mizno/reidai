module SimpleViewer
    (
     ViewInfo(..),
     putAxisNames,
     viewVertex3,
     viewVertex,
     viewRenderString3,
     viewRenderString,
     viewCreateWindow,
     viewRenderFunc,
     viewRenderFunc2D,
     viewPlotDots3D,
     viewPlotDots2D,
     d2f
     ) where

import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import Control.Monad
import Control.Applicative
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Foreign.C.Types

d2f :: (Double -> Double) -> (GLfloat -> GLfloat)
d2f f = g
   where
     g x  = Foreign.C.Types.CFloat ((realToFrac (f (realToFrac x)::Double))::Float)


data ViewInfo = ViewInfo {
      -- for model view
      trans :: IORef (Vector3 GLfloat),
      rot :: IORef (Vector3 GLfloat),
      zoom :: IORef GLfloat,

      -- keyboard mouse state
      lastPositionOnPlane :: IORef Position,
      modifiers :: IORef Modifiers,
      mouseMode :: IORef MouseMode,

      -- graph size
      u_max :: IORef (Vector3 GLfloat),
      u_min :: IORef (Vector3 GLfloat),
      -- world size
      r_max :: IORef (Vector3 GLfloat),
      r_min :: IORef (Vector3 GLfloat)
    }

data MouseMode = NoMode | TransMode | RotMode | ZoomMode

initialTrans = Vector3 0 0 0 :: Vector3 GLfloat
initialRot = Vector3 0 0 0 :: Vector3 GLfloat
initialZoom =  1 :: GLfloat

initial_u_max = Vector3 1 1 1 :: Vector3 GLfloat
initial_u_min = Vector3 (-1) (-1) (-1) :: Vector3 GLfloat

initial_r_max = Vector3 1 1 1 :: Vector3 GLfloat
initial_r_min = Vector3 0 0 0 :: Vector3 GLfloat

makeViewInfo :: IO ViewInfo
makeViewInfo = do
  tr <- newIORef initialTrans
  ro <- newIORef initialRot
  zo <- newIORef initialZoom
  la <- newIORef (Position (-1) (-1))
  mo <- newIORef (Modifiers Up Up Up)
  mm <- newIORef NoMode
  uma <- newIORef initial_u_max
  umi <- newIORef initial_u_min
  rma <- newIORef initial_r_max
  rmi <- newIORef initial_r_min
  return $ ViewInfo {
                 trans = tr,
                 rot = ro,
                 zoom = zo,
                 lastPositionOnPlane = la,
                 modifiers = mo,
                 mouseMode = mm,
                 u_max = uma,
                 u_min = umi,
                 r_max = rma,
                 r_min = rmi
               }

viewInit :: IO ()
viewInit = do
   -- select clearing color
   clearColor $= Color4 0 0 0 0

   -- initialize viewing values
   matrixMode $= Projection
   loadIdentity
--   ortho 0 1 0 1 (-1) 10
   ortho 0 1 0 1 40 60

r2u :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat
r2u uma umi rma rmi r = (r - rmi) / (rma - rmi) * (uma - umi) + umi

r2u3IO :: ViewInfo -> Vector3 GLfloat -> IO (Vector3 GLfloat)
r2u3IO view (Vector3 rx ry rz) = do
  Vector3 x_uma y_uma z_uma <- get (u_max view)
  Vector3 x_umi y_umi z_umi <- get (u_min view)
  Vector3 x_rma y_rma z_rma <- get (r_max view)
  Vector3 x_rmi y_rmi z_rmi <- get (r_min view)
  let ans = Vector3
         (r2u x_uma x_umi x_rma x_rmi rx)
         (r2u y_uma y_umi y_rma y_rmi ry)
         (r2u z_uma z_umi z_rma z_rmi rz)
  return ans

max3 :: (Ord a) => a -> a -> a-> a
max3 x y z = max (max x y) z

min3 :: (Ord a) => a -> a -> a-> a
min3 x y z = min (min x y) z

updateMaxMin :: ViewInfo -> Vector3 GLfloat -> IO()
updateMaxMin view v@(Vector3 x y z) = do
  rma@(Vector3 x_rma y_rma z_rma) <- get (r_max view)
  rmi@(Vector3 x_rmi y_rmi z_rmi) <- get (r_min view)
  let n_x_max = max x x_rma
  let n_y_max = max y y_rma
  let n_z_max = max z z_rma
  let n_x_min = min x x_rmi
  let n_y_min = min y y_rmi
  let n_z_min = min z z_rmi
  let mMax = max3 n_x_max n_y_max n_z_max
  let mMin = min3 n_x_min n_y_min n_z_min
--  writeIORef (r_max view) $ Vector3 n_x_max n_y_max n_z_max
--  writeIORef (r_min view) $ Vector3 n_x_min n_y_min n_z_min
  writeIORef (r_max view) $ scalarProduct mMax (Vector3 1 1 1)  -- 3軸全部同じ単位のレンジのしたいときは
  writeIORef (r_min view) $ scalarProduct mMin (Vector3 1 1 1)  -- 上の2行を消してこちらを実行

viewVertex3 :: ViewInfo -> Vector3 GLfloat -> IO()
viewVertex3 view v = do
  updateMaxMin view v
  (Vector3 ux uy uz) <- r2u3IO view v
  vertex $ Vertex3  ux uy uz

viewVertex :: ViewInfo -> GLfloat -> GLfloat -> GLfloat -> IO()
viewVertex view x y z = viewVertex3 view (Vector3 x y z)

viewRenderString3 :: ViewInfo -> Vector3 GLfloat -> String -> IO()
viewRenderString3 view v str = do
  u@(Vector3 x y z) <- r2u3IO view v
  preservingMatrix $ do
    currentRasterPosition $= Vertex4 x y z 1
    renderString Fixed8By13 $ str

viewRenderString :: ViewInfo -> GLfloat -> GLfloat -> GLfloat -> String -> IO()
viewRenderString view x y z str = viewRenderString3 view (Vector3 x y z) str

cutDecimal :: (RealFrac a, Integral b)=> a -> b
cutDecimal x = 
    if x >= 0
    then floor x
    else ceiling x

divList :: GLfloat -> GLfloat -> [GLfloat]
divList ami ama = let
    mi = min 0 (min (ami * 1.2) (ama * 1.2))
    ma = max 0 (max (ami * 1.2) (ama * 1.2))
    rFunc = (\x -> floor $ logBase 10 x)
    intLog = (fromIntegral $rFunc (max (abs ma) (abs mi))) 
    range = 10 ** intLog
    intLogList = ([(cutDecimal (mi/range)) .. (cutDecimal(ma/range))]) 
  in
    map (range *) $ map fromInteger intLogList

subDivList :: [GLfloat] -> [GLfloat]
subDivList [] = []
subDivList [x] = []
subDivList (x0:x1:xs) = ((x1 - x0)/2 + x0) : subDivList (x1:xs)

scalarProduct :: GLfloat -> Vector3 GLfloat -> Vector3 GLfloat
scalarProduct a (Vector3 x y z) = Vector3 (a*x) (a*y) (a*z)

putLineLegend :: Color3 GLfloat -> Vector3 GLfloat -> String -> ViewInfo -> IO()
putLineLegend c v@(Vector3 x y z) str view = do
              color $ c
              renderPrimitive Lines $ do
                              viewVertex view x y z
                              viewVertex view (1.1*x) y z
              viewRenderString view (1.105 * x) y z str
putDotLegend :: Color3 GLfloat -> Vector3 GLfloat -> String -> ViewInfo -> IO()
putDotLegend c v@(Vector3 x y z) str view = do
              color $ c
              renderPrimitive Points $ do
                              viewVertex view (1.05 * x) y z
              viewRenderString view (1.105 * x) y z str

putAxisNames :: [String] -> ViewInfo -> IO()
putAxisNames (x:y:z:[]) view = do
             color $ ((Color3 1 1 1)::(Color3 GLfloat))
             v@(Vector3 x_rmax y_rmax z_rmax) <- get(r_max view)
             v@(Vector3 x_rmin y_rmin z_rmin) <- get(r_min view)
             viewRenderString3 view (Vector3 (1.02 * (x_rmax - x_rmin) + x_rmin) 0 0) x
             viewRenderString3 view (Vector3 0 (1.02 * (y_rmax - y_rmin) + y_rmin) 0) y
             viewRenderString3 view (Vector3 0 0 (1.02 * (z_rmax - z_rmin) + z_rmin)) z
putAxisNames _ _ = return ()
             
drawAxis :: ViewInfo -> IO ()
drawAxis view = do
  rma@(Vector3 x_rma y_rma z_rma) <- get (r_max view)
  rmi@(Vector3 x_rmi y_rmi z_rmi) <- get (r_min view)
  let xs = divList x_rma x_rmi
  let ys = divList y_rma y_rmi
  let zs = divList z_rma z_rmi
  let sxs = subDivList xs
  let sys = subDivList ys
  let szs = subDivList zs
  -- put numbers
  forM xs $ \x ->
       viewRenderString3 view (Vector3 x 0 0) (show x)
  forM ys $ \y ->
       viewRenderString3 view (Vector3 0 y 0) (show y)
  forM zs $ \z ->
       viewRenderString3 view (Vector3 0 0 z) (show z)
  -- render mesh
  color $ ((Color4 0.3 0.5 0.5 0.1) :: Color4 GLfloat)
  forM (xs++sxs) $ \x ->
       forM (ys++sys) $ \y ->
            renderPrimitive Lines $ do
                viewVertex view x 0 0
                viewVertex view x y 0
                viewVertex view 0 y 0
                viewVertex view x y 0
  forM (zs++szs) $ \z ->
       forM (xs++sxs) $ \x ->
            renderPrimitive Lines $ do
                viewVertex view x 0 0
                viewVertex view x 0 z
                viewVertex view 0 0 z
                viewVertex view x 0 z
  -- long axis
  color $ ((Color4 1 1 1 1) :: Color4 GLfloat)
  renderPrimitive Lines $ do
             viewVertex view (max x_rma 0) 0 0
             viewVertex view (min x_rmi 0) 0 0
             viewVertex view 0 (max y_rma 0) 0
             viewVertex view 0 (min y_rmi 0) 0
             viewVertex view 0 0 (max z_rma 0)
             viewVertex view 0 0 (min z_rmi 0)

frameDisplay ::  ViewInfo -> [(ViewInfo -> DisplayCallback)] -> DisplayCallback
frameDisplay view aDisplays = do
  lTrans <- get (trans view)
  loadIdentity
  translate (Vector3 0 0 (-5 :: GLfloat))
  translate lTrans

  rv@(Vector3 xRot yRot zRot) <- get (rot view)
  rotate yRot (Vector3 1 0 0)
  rotate xRot (Vector3 0 1 0)
  rotate zRot (Vector3 0 0 1)

  lzoom <- get (zoom view)
  scale lzoom lzoom lzoom 

  clear [ ColorBuffer, DepthBuffer ]
  color $ ((Color3 0 1 0) :: Color3 GLfloat)
  mapM_ (\disp -> disp view) aDisplays
  color $ ((Color3 1 1 1) :: Color3 GLfloat)
  drawAxis view

  flush


reshape ::  ReshapeCallback
reshape size@(Size w h) = do
   let vp = 0.8
       aspect = fromIntegral w / fromIntegral h

   viewport $= (Position 0 0, size)

   matrixMode $= Projection
   loadIdentity
   frustum (-vp) vp (-vp / aspect) (vp / aspect) 3 10

   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-5 :: GLfloat))


motion :: ViewInfo -> MotionCallback
motion view p = do
  mode <- get (mouseMode view)
  modeMotion mode view p
  postRedisplay Nothing

modeMotion :: MouseMode -> ViewInfo -> MotionCallback
modeMotion TransMode view p@(Position x y) = do
  Position lx ly <- get (lastPositionOnPlane view)
  let dt = Vector3 (0.01*(fromIntegral (x - lx))) (0.01*(fromIntegral (ly - y))) 0
  lTrans <- get (trans view)
  writeIORef (trans view) $ liftA2 (+) lTrans dt
  writeIORef (lastPositionOnPlane view) $ Position x y
modeMotion RotMode view p@(Position x y) = do
  Position lx ly <- get (lastPositionOnPlane view)
  let dt = Vector3 (0.5*(fromIntegral (x - lx))) (0.5*(fromIntegral (y - ly))) 0
  lRot <- get (rot view)
  writeIORef (rot view) $ liftA2 (+) lRot dt
  writeIORef (lastPositionOnPlane view) $ Position x y
modeMotion ZoomMode view p@(Position x y) = do
  Position lx ly <- get (lastPositionOnPlane view)
  let dt = 0.01*(fromIntegral (ly - y))
  lZoom <- get (zoom view)
  writeIORef (zoom view) $ lZoom + dt
  writeIORef (lastPositionOnPlane view) $ Position x y
modeMotion NoMode view p@(Position x y) = do
  print "non"

keyboard :: ViewInfo -> KeyboardMouseCallback
keyboard _ (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard view (MouseButton button) Down _ p = do
    writeIORef (mouseMode view) $ mouse2Mode button
    writeIORef (lastPositionOnPlane view) $ p
keyboard _ _ _ _ _ = return  ()

mouse2Mode :: MouseButton -> MouseMode
mouse2Mode LeftButton = TransMode
mouse2Mode MiddleButton = ZoomMode
mouse2Mode RightButton = RotMode
mouse2Mode _ = NoMode

viewCreateWindow :: String -> [(ViewInfo -> DisplayCallback)] -> IO ViewInfo
viewCreateWindow title dispFuncs = do
  getArgsAndInitialize
  view <- makeViewInfo
  initialDisplayMode $= [ SingleBuffered, RGBAMode ]
  initialWindowSize $= Size 400 400
  initialWindowPosition $= Position 100 100
  createWindow title
  viewInit
  displayCallback $= frameDisplay view dispFuncs
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboard view)
  motionCallback $= Just (motion view)
  return view

viewPlotDots3D :: [(Vector3 GLfloat, Color3 GLfloat)] -> ViewInfo -> IO()
viewPlotDots3D [] _ = return ()
viewPlotDots3D ((xyz,c):xyzscs) view = do
               pointSize $= 10
               color $ c
               renderPrimitive Points $ do
                               viewVertex3 view xyz
               viewPlotDots3D xyzscs view

viewPlotDots2D :: [(Vector2 GLfloat, Color3 GLfloat)] -> ViewInfo -> IO()
viewPlotDots2D [] _ = return ()
viewPlotDots2D ((Vector2 x y, c):xyzscs) view = do
               pointSize $= 10
               color $ c
               renderPrimitive Points $ do
                               viewVertex view x y 0
               viewPlotDots2D xyzscs view

ftPair :: [a] -> [(a, a)]
ftPair [] = []
ftPair (t:[]) = []
ftPair (f:t:xs) = (f, t) : ftPair (t:xs)

viewRenderFunc2D :: (GLfloat -> GLfloat) -> (Color3 GLfloat) -> GLfloat -> GLfloat -> Integer -> ViewInfo -> IO()
viewRenderFunc2D f c xMin xMax n view = do
  color c
  let is = (map fromIntegral [0..n])::[GLfloat]
  let xs = map (\i -> (xMax - xMin) / (fromIntegral n) * i + xMin) is
  forM_ (ftPair xs) $ \(x1, x2) -> 
       renderPrimitive Lines $ do
                       viewVertex view x1 (f x1) 0
                       viewVertex view x2 (f x2) 0


viewRenderFunc :: (GLfloat -> GLfloat -> GLfloat) -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Integer -> ViewInfo -> IO()
viewRenderFunc f xMin xMax yMin yMax n view = do
  putAxisNames ["x", "f(x,y)", "y"] view
  color $ ((Color3 0 0.5 0) :: Color3 GLfloat)
  (Vector3 _ yma _) <- get (r_max view)
  (Vector3 _ ymi _) <- get (r_min view)
  let colorFunc = (\y -> (y - ymi)/(yma - ymi))
  let vertexAndColor = (\x y z v -> do {color $ ((Color3 (colorFunc y) 0.5 (1 - (colorFunc y))) :: Color3 GLfloat);viewVertex v x y z})
  let is = (map fromIntegral [0..n])::[GLfloat]
  let xs = map (\i -> (xMax - xMin) / (fromIntegral n) * i + xMin) is
  let ys = map (\i -> (yMax - yMin) / (fromIntegral n) * i + yMin) is
--  _drawFuncLoopX f xs ys view
  forM_ (ftPair xs) $ \(x0, x1) ->
    forM_ (ftPair ys) $ \(y0, y1) ->
            preservingMatrix $ do
                renderPrimitive LineStrip $ do
                       vertexAndColor x0 (f x0 y0) y0 view
                       vertexAndColor x0 (f x0 y1) y1 view
                       vertexAndColor x1 (f x1 y1) y1 view
                       vertexAndColor x1 (f x1 y0) y0 view
                       vertexAndColor x0 (f x0 y0) y0 view

------------------------------------------------------------


-- example

--testPoints :: [Vector3 GLfloat]
testPoints = [(Vector3 (10*x) (0.5*x*y) (10*y), Color3 0 1 0) | x <- ([(-9)..9]::[GLfloat]), y <- ([(-9)..9]::[GLfloat])]
testPoints2 = [(Vector2 (10*x) (100* (sin x)) , Color3 0 1 0) | x <- ([(-9)..9]::[GLfloat])]


main :: IO ()
main = do
  viewCreateWindow "Test" $ 
        [
         putAxisNames ["x", "y", ""]
        , putLineLegend (Color3 0 1 0) (Vector3 90 110 0) "interpolation"
        , putDotLegend (Color3 0 1 0) (Vector3 90 120 0) "given points"
        , viewPlotDots2D testPoints2
        , viewRenderFunc2D (\x -> 100*(sin (x/10))) (Color3 1 1 0) (-100) (100) 100
        ]
  mainLoop


