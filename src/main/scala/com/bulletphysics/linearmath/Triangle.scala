package com.bulletphysics.linearmath

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
}