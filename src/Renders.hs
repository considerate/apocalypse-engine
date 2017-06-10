module Renders where
import Graphics.UI.GLUT

renderVertex :: (GLfloat,GLfloat,GLfloat) -> IO ()
renderVertex (x,y,z) = vertex $ Vertex3 x y z

renderPoints :: Foldable t => PrimitiveMode -> t (GLfloat, GLfloat, GLfloat) -> IO ()
renderPoints shape points =
    renderPrimitive shape $ mapM_ renderVertex points

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

