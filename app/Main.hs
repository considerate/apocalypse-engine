module Main where
import Graphics.UI.GLUT
import Data.Maybe(catMaybes)

type Vec3 = (GLfloat, GLfloat, GLfloat)

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

(|-) :: Vec3 -> Vec3 -> Vec3
(x1,y1,z1) |- (x2,y2,z2) = (x1 - x2, y1 - y2, z1 - z2)
(|+) :: Vec3 -> Vec3 -> Vec3
(x1,y1,z1) |+ (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)
(|.) :: Vec3 -> Vec3 -> GLfloat
(x1,y1,z1) |. (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

(|.*) :: Vec3 -> GLfloat -> Vec3
(x,y,z) |.* t = (x*t, y*t, z*t)

(|#) :: Vec3 -> Vec3 -> Vec3
(ax,ay,az) |# (bx,by,bz)
    = ( ay*bz - az*by
      , az*bx - ax*bz
      , ax*by - ay*bx
      )
vectorLength :: Vec3 -> GLfloat
vectorLength v = sqrt $ v |. v


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

renderVertex :: (GLfloat,GLfloat,GLfloat) -> IO ()
renderVertex (x,y,z) = vertex $ Vertex3 x y z

renderPoints :: Foldable t => PrimitiveMode -> t (GLfloat, GLfloat, GLfloat) -> IO ()
renderPoints shape points =
    renderPrimitive shape $ mapM_ renderVertex points

rayLineIntersect :: (Vec3, Vec3) -> (Vec3, Vec3) -> Maybe GLfloat
rayLineIntersect (o, d) (a, b)
    = if t2 >= 0 && t2 <= 1 && t1 >= 0
        then Just t1
        else Nothing
    where
        (dx, dy, 0) = d
        v1 = o |- a
        v2 = b |- a
        v3 = (-dy, dx, 0)
        t1 = vectorLength (v2 |# v1) / (v2 |. v3)
        t2 = (v1 |. v3) / (v2 |. v3)

intersectionPoint :: (Vec3, Vec3) -> GLfloat -> Vec3
intersectionPoint (o,d) t
    = point
    where
        point = o |+ (d |.* t)

circlePoints :: (Num t, Floating t1, Enum t1) => (t1, t1) -> t1 -> t1 -> [(t1, t1, t)]
circlePoints (x,y) radius number
 = [let alpha = twoPi * i /number
    in  (x + radius * sin alpha, y + radius * cos alpha,0)
   |i <- [1,2..number]]
  where
    twoPi = 2*pi
circle :: (Num t, Enum t1, Floating t1) => (t1, t1) -> t1 -> [(t1, t1, t)]
circle (x,y) radius = circlePoints (x,y) radius 100
fillCircle :: (GLfloat, GLfloat) -> GLfloat -> IO ()
fillCircle (x,y) r = renderPoints Polygon (circle (x,y) r)

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  mapM_ print intersections
  renderPoints Lines ground
  currentColor $= Color4 1 0 0 1
  mapM_ (\(x,y,_) -> fillCircle (x,y) 0.01) points
  --renderPoints points
  flush
  where
    frameTime = 0.5 :: GLfloat
    o = (0,0,0) :: Vec3
    d = (2,-2,0) :: Vec3
    groupLines [] = []
    groupLines [_] = []
    groupLines (x:y:rest) = (x,y) : groupLines rest
    lineSegments = groupLines ground
    intersections = catMaybes $ fmap (rayLineIntersect (o,d)) lineSegments :: [GLfloat]
    points = fmap (intersectionPoint (o,d)) (filter (< frameTime) intersections) :: [Vec3]


