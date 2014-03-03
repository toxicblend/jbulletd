package com.bulletphysics.linearmath

import javax.vecmath.Tuple3d
import javax.vecmath.Vector3d
import javax.vecmath.Point3d

class AABB(val aabbMin:Vector3d, val aabbMax:Vector3d){
  /**
   * create new zero extend AABB with center at (0,0,0)
   */
  def this() = this(new Vector3d, new Vector3d)
  
  def this(aabb:AABB) = this(new Vector3d(aabb.aabbMin), new Vector3d(aabb.aabbMax))
  
  def this(aabbs:Iterable[AABB]) = {
    this(aabbs.head)
    aabbs.tail.foreach(aabb => this.union(aabb))
  }

  /**
   * create new zero extend AABB with center at @origin
   */
  def this(origin:Tuple3d) = this(new Vector3d(origin), new Vector3d(origin))
  
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
  
  def copy:AABB = {
    new  AABB(new Vector3d(aabbMin), new Vector3d(aabbMax)) 
  }
  
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
  }
  
  override def toString = {
    "Min:" +aabbMin.toString + " Max:" + aabbMax
  }
}