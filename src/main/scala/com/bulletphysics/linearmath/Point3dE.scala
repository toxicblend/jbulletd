package com.bulletphysics.linearmath

import javax.vecmath.Point3d
import javax.vecmath.Tuple3d
import javax.vecmath.Tuple3f

/**
 * Extends Point3d with some methods from Tuple3dTrait
 */
class Point3dE(__x:Double,__y:Double,__z:Double) extends Point3d(__x,__y,__z) with Tuple3dTrait {
  def this() = this(0,0,0)
  def this(__that:Tuple3d) = this(__that.x,__that.y,__that.z)
  def this(__that:Tuple3f) = this(__that.x,__that.y,__that.z)
  def this(x:IndexedSeq[Double]) = this(x(0),x(1),x(2))
}
