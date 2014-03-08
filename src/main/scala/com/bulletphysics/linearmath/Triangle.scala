package com.bulletphysics.linearmath

import com.bulletphysics.BulletGlobals

class Triangle(val a:Point3dE, val b:Point3dE, val c:Point3dE) {
  
  def this() = this(new Point3dE,new Point3dE,new Point3dE)
  
  @inline
  def setSelf(a:Point3dE, b:Point3dE, c:Point3dE):Triangle = {
    this.a.set(a)
    this.b.set(b)
    this.c.set(c)
    this
  }
  
  @inline
  def computeCentroid(result:Point3dE):Point3dE = {
    result.setSelf(a).addSelf(b).addSelf(c).scaleSelf(1d/3d)
  }

  @inline
  def computeNormal(result:Vector3dE) {
    // normal = a.sub(c).crossSelf(a.sub(b)).normalize();
    val tmp = new Vector3dE(a).subSelf(b)
    result.setSelf(a).subSelf(c).crossSelf(tmp).normalizeSelf
  }
  
  override
  def toString = {
    "a:" + a + " b:" + b + " c:" + c
  }
  
  /**
   * Checks if the given XY point is inside the triangle created by the points a(x,y),
   * b(x,y) and c(x,y). The triangle vertices are inclusive themselves.
   * 
   * This is a direct port from toxi.geom.Triangle2D
   * 
   * @return true, if point is in triangle.
   */
 def containsXYPoint(p:Point3dE) = {
   val v1 = new Vector3dE(p).subSelf(a)
   val v2 = new Vector3dE(p).subSelf(b)
   val v3 = new Vector3dE(p).subSelf(c)
   v1.z = 0d; v2.z = 0d; v3.z = 0d
   
   if (v1.isZeroVector || v2.isZeroVector || v3.isZeroVector) {
     true
   } else {
      v1.normalize
      v2.normalize
      v3.normalize
      val total_angles = math.acos(v1.xyDot(v2)) + math.acos(v2.xyDot(v3)) + math.acos(v3.xyDot(v1))
      val diff = math.abs(total_angles - BulletGlobals.SIMD_2_PI)
      diff <= 0.001
    }
  }
}