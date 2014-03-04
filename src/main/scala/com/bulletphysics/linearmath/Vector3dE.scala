package com.bulletphysics.linearmath
import javax.vecmath.Tuple3d
import javax.vecmath.Tuple3f
import javax.vecmath.Vector3d

/**
 * Extends Vector3d with some methods from Tuple3dTrait
 */
class Vector3dE(__x:Double,__y:Double,__z:Double) extends Vector3d(__x,__y,__z) with Tuple3dTrait {
  def this(__that:Tuple3d) = this(__that.x,__that.y,__that.z)
  def this(__that:Tuple3f) = this(__that.x,__that.y,__that.z)
  def this(x:IndexedSeq[Double]) = this(x(0),x(1),x(2))
}