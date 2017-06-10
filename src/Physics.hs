module Physics where
import Graphics.UI.GLUT
import Vec3

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

