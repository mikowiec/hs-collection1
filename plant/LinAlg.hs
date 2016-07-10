
module LinAlg where

import Types
import Debug.QuickCheck

origo :: Point
origo = (0.0, 0.0, 0.0)

dirup :: Vector
dirup = (0.0, 1.0, 0.0)


--- Vector operations

add :: Vector -> Vector -> Vector
add (x1,x2,x3) (y1,y2,y3) = (x1+y1,x2+y2,x3+y3)

sub :: Vector -> Vector -> Vector
sub (x1,x2,x3) (y1,y2,y3) = (x1-y1,x2-y2,x3-y3)

dot :: Vector -> Vector -> Float
dot (x1,x2,x3) (y1,y2,y3) = (x1*y1 + x2*y2 + x3*y3)

mulk :: Float -> Vector -> Vector
mulk k (x1,x2,x3) = (x1*k,x2*k,x3*k)

cmul :: Vector -> Vector -> Vector
cmul (x1,x2,x3) (y1,y2,y3) = (x1*y1, x2*y2, x3*y3)

cross :: Vector -> Vector -> Vector
cross (x1,x2,x3) (y1,y2,y3) = (x2*y3-x3*y2, x3*y1-x1*y3, x1*y2-x2*y1)

ang :: Vector -> Vector -> Float
ang u v = acos $ min 1.0 $ dot (norm u) (norm v)

absval :: Vector -> Float
absval u = sqrt $ dot u u

norm :: Vector -> Vector
norm (a, b, c)
 | len == 0.0 = (a,b,c)
 | otherwise  = (a/len, b/len, c/len)
 where len = (sqrt (a*a + b*b + c*c))


rot_vec_axis :: Vector -> Vector -> Float -> Vector
rot_vec_axis vec axis angle =
    rot_vec_q vec (rot_axis axis angle)


rot :: Vector -> Vector -> Vector -> Vector
rot vec c1 c2 =
    rot_vec_axis vec (norm (cross c1 c2)) (ang c1 c2)

emul (x1,x2,x3) (y1,y2,y3) = (x1*y1, x2*y2, x3*y3)

--- Quaternion operations


addq :: Quat -> Quat -> Quat
addq (q1,w1) (q2,w2) = (add q1 q2, w1+w2)

mulq :: Quat -> Quat -> Quat
mulq (q1,w1) (q2,w2) = (((cross q1 q2) `add` (mulk w2 q1)) `add` (mulk w1 q2), w1*w2 - dot q1 q2)

cmulq (q1,w1) (q2,w2) = (cmul q1 q2, w1*w2)

mulkq :: Float -> Quat -> Quat
mulkq k (q,w) = (mulk k q, k*w)

id_quat :: Quat
id_quat = rot_axis (1,0,0) 0


normq :: Quat -> Float
normq ((x1,x2,x3), x4) = sqrt $ x1*x1 + x2*x2 + x3*x3 + x4*x4

conj :: Quat -> Quat
conj ((x1,x2,x3), x4) = (((-x1),(-x2),(-x3)), x4)

inv :: Quat -> Quat
inv q = mulkq (1 / (normq q)) (conj q)

slerp :: Quat -> Quat -> Float -> Quat
slerp q q' _ | q == q' = q
slerp q q' t = 
           (((sin (phi * (1-t))) / sin phi) `mulkq` q) `addq`
           (((sin (phi * t)) / sin phi) `mulkq` q')
 where phi = acos $ (\((a,b,c),d) -> a+b+c+d) (cmulq q q')



--- Combined Vector/Quaternion


rot_q_y :: Vector -> Quat
rot_q_y dir =
    let a = ang dirup dir in
    case (abs a < 0.0001, abs a > 3.141) of
     (True,_) -> id_quat
     (_,True) -> rot_axis (1,0,0) pi
     _        -> rot_axis (norm (cross dirup dir)) a

rot_axis :: Vector -> Float -> Quat
rot_axis axis angle = let theta = angle / 2 in (mulk (sin theta) axis, cos theta)

q2axis :: Quat -> (Float,Vector)
q2axis (v,a)
 | v == (0,0,0) || a == 1 = (0,(1,0,0))
 | otherwise =
    let theta = acos a in
    (2*theta, (1 / (sin theta)) `mulk` v)

-- test:  (q2axis . rot_axis) =~= id
prop_q2q_iso1 x1 x2 x3 v = 
    let a = (acos.cos) v
        axis = norm (x1,x2,x3)
        (a',axis') = q2axis $ rot_axis axis a
    in (a /= 0 && absval axis > 0.1) ==>
            ((a `aeq` a' && axis `aveq` axis') ||
             (a `aeq` (-a') && axis `aveq` ((-1) `mulk` axis')))

aeq f1 f2 = abs (f1 - f2) < 0.0001
aveq f1 f2 = absval (f1 `sub` f2) < 0.0001

v2q :: Vector -> Quat
v2q v = (v,0)
q2v :: Quat -> Vector
q2v (v,_) = v


rot_vec_q :: Vector -> Quat -> Vector
rot_vec_q vec q =
    q2v $ q `mulq` (v2q vec) `mulq` (inv q)


mk_rot_axis :: Vector -> Vector -> (Float, Vector)
mk_rot_axis v1 v2
 | abs a < 0.0001 || abs a > (pi-0.0001) = (0, (1,0,0))
 | otherwise = (a, norm (cross v1 v2))
 where a = ang v1 v2

mk_rot_quat :: Vector -> Vector -> Quat
mk_rot_quat v1 v2 = rot_axis v a
    where (a,v) = mk_rot_axis v1 v2


mk_axises :: Vector -> (Vector,Vector)
mk_axises n
 | absval a1 > 0.1 = (norm a1,a2n)
 | otherwise       = (norm b1,b2n)
 where (a1,a2n) = (n `cross` (1,0,0), norm $ n `cross` a1)
       (b1,b2n) = (n `cross` (0,1,0), norm $ n `cross` b1)



d2r d = d * pi / 180
r2d r = r * 180 / pi

