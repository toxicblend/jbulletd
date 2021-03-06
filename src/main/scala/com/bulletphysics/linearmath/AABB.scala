package com.bulletphysics.linearmath

import javax.vecmath.Tuple3d
import javax.vecmath.Vector3d
import javax.vecmath.Point3d

class AABB(val aabbMin:Point3d, val aabbMax:Point3d){
  /**
   * create new zero extend AABB with center at (0,0,0) and minus Double.MaxValue extent
   */
  def this() = this(new Point3d(AABB.UNINITIALIZED_AABBMIN), new Point3d(AABB.UNINITIALIZED_AABBMAX))
  
  def this(aabb:AABB) = this(new Point3d(aabb.aabbMin), new Point3d(aabb.aabbMax))
   
  def this(aabbs:Iterable[AABB]) = {
    this(aabbs.head)
    aabbs.tail.foreach(aabb => this.union(aabb))
  }

  /**
   * create new zero extend AABB with center at @origin
   */
  def this(origin:Tuple3d) = this(new Point3d(origin), new Point3d(origin))
  
  def aabbExpand(point:Tuple3d) = {
    if (point.x < aabbMin.x) aabbMin.x = point.x
    if (point.y < aabbMin.y) aabbMin.y = point.y
    if (point.z < aabbMin.z) aabbMin.z = point.z
    if (point.x > aabbMax.x) aabbMax.x = point.x   
    if (point.y > aabbMax.y) aabbMax.y = point.y
    if (point.z > aabbMax.z) aabbMax.z = point.z
  }
  
  def getMin = aabbMin
  def getMax = aabbMax
  
  def copy:AABB = new  AABB(new Point3d(aabbMin), new Point3d(aabbMax))
  
  /**
   * sets this AABB as the union of this and the other AABB
   */
  def union(that:AABB) = {
    if (that.aabbMin.x < aabbMin.x) aabbMin.x = that.aabbMin.x
    if (that.aabbMin.y < aabbMin.y) aabbMin.y = that.aabbMin.y
    if (that.aabbMin.z < aabbMin.z) aabbMin.z = that.aabbMin.z
    if (that.aabbMax.x > aabbMax.x) aabbMax.x = that.aabbMax.x
    if (that.aabbMax.y > aabbMax.y) aabbMax.y = that.aabbMax.y
    if (that.aabbMax.z > aabbMax.z) aabbMax.z = that.aabbMax.z
    this
  }
  
  def reset = {
    aabbMin.set(AABB.UNINITIALIZED_AABBMIN)
    aabbMax.set(AABB.UNINITIALIZED_AABBMAX)
    this
  }
  
  override def toString = {
    "Min:" +aabbMin.toString + " Max:" + aabbMax
  }
}

object AABB {
  val UNINITIALIZED_AABBMIN = new Point3d(Double.PositiveInfinity,Double.PositiveInfinity,Double.PositiveInfinity)
  val UNINITIALIZED_AABBMAX = new Point3d(Double.NegativeInfinity,Double.NegativeInfinity,Double.NegativeInfinity)
}