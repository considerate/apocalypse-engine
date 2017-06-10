module Vec3 where
import Graphics.UI.GLUT

type Vec3 = (GLfloat, GLfloat, GLfloat)

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
