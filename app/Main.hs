module Main where
import Graphics.UI.GLUT
import Data.Maybe(catMaybes)
import Vec3
import Renders
import Physics

myPoints :: [Vec3]
myPoints = fmap (\k -> (sin (2*pi*k/12), cos (2*pi*k/12), 0)) [1..12]

ground :: [Vec3]
ground = interleave coords
    where
        interleave [] = []
        interleave [x] = [x]
        interleave (x:y:rest) = x : y : interleave (y:rest)
        scaling = 10
        coords = fmap (p . shrink) [-scaling,-(scaling-1)..scaling]
        shrink k = k/scaling
        p k = (k, k * k - 0.8, 0)

size :: Size
size = Size 640 480

main :: IO ()
main = do
  getArgsAndInitialize
  createWindow "Hello World"
  -- windowSize $= size
  fullScreen
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  mapM_ print intersections
  renderPoints Lines ground
  currentColor $= red
  mapM_ (\(x,y,_) -> fillCircle (x,y) 0.01) points
  flush
  where
    red = Color4 1 0 0 1
    frameTime = 0.5 :: GLfloat
    o = (0,0,0) :: Vec3
    d = (2,-2,0) :: Vec3
    groupLines [] = []
    groupLines [_] = []
    groupLines (x:y:rest) = (x,y) : groupLines rest
    lineSegments = groupLines ground
    intersections = catMaybes $ fmap (rayLineIntersect (o,d)) lineSegments :: [GLfloat]
    points = fmap (intersectionPoint (o,d)) (filter (< frameTime) intersections) :: [Vec3]


