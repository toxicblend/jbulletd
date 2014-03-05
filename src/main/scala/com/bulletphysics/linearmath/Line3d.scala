package com.bulletphysics.linearmath

/**
 * origin is read only
 * dir may get normalized
 */
class Line3d(val origin:Point3dE, val dir:Vector3dE) { 
  def this() = this(new Point3dE, new Vector3dE(Vector3dE.VECTOR001))
  def this(origin:Point3dE) = this(origin, new Vector3dE(Vector3dE.VECTOR001))
  def this(dir:Vector3dE) = this(new Point3dE, dir)
  
  @inline
  def setSelf(fromP:Point3dE, toP:Point3dE) = {
    origin.set(fromP)
    dir.setSelf(toP).subSelf(fromP)
  }
  
  @inline
  def getPointAtDistance(result:Point3dE, dist:Double) = {
    result.setSelf(dir).scaleSelf(dist).addSelf(origin)
  }
  
  /**
   * returns a plane perpendicular to the XY plane that contains this ray.
   * plane.origin = ray.origin
   * plane.normal = ray.dir.cross(0,0,1)
   * 
   * TODO: handle the dir = (0,0,+-1) case
   */
  @inline
  def getZPlane(result:Plane) = {
    result.origin.set(origin)
    result.normal.setSelf(dir).crossSelf(Vector3dE.VECTOR001)
    result
  }
  
  override
  def toString = {
    "o:" + origin + " d:" + dir
  }
}