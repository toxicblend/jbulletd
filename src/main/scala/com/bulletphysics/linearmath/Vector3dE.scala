package com.bulletphysics.linearmath
import javax.vecmath.Tuple3d
import javax.vecmath.Tuple3f
import javax.vecmath.Vector3d

/**
 * Extends Vector3d with some methods from Tuple3dTrait
 */
class Vector3dE(__x:Double,__y:Double,__z:Double) extends Vector3d(__x,__y,__z) with Tuple3dTrait {
  def this() = this(0,0,0)
  def this(__that:Tuple3d) = this(__that.x,__that.y,__that.z)
  def this(__that:Tuple3f) = this(__that.x,__that.y,__that.z)
  def this(x:IndexedSeq[Double]) = this(x(0),x(1),x(2))
  
  @inline
  def normalizeSelf = {
    normalize
    this
  }
  
  @inline
  def crossSelf(v:Tuple3d) = {
    val cx = y*v.z-v.y*z
    val cy = z*v.x-v.z*x
    z = x*v.y-v.x*y
    y = cy
    x = cx
    this
  }
  
  /**
   * Returns the dot product of this vector and vector v1 if they were two dimensional
   * @param v1 the other vector
   * @return the xy dot product of this and v1
   */
  def xyDot(v1:Vector3d) = {
    this.x*v1.x + this.y*v1.y
  }
}



object Vector3dE {
  // 'readonly' vetors, don't change the content of these
  val VECTOR100 = new Vector3dE(1,0,0)
  val VECTOR010 = new Vector3dE(0,1,0)
  val VECTOR001 = new Vector3dE(0,0,1)
}