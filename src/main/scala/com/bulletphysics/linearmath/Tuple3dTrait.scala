package com.bulletphysics.linearmath
import javax.vecmath.Tuple3d
import javax.vecmath.Tuple3f

trait Tuple3dTrait extends Tuple3d {
   
  /**
   * same as scale, it only returns 'this'
   */
  @inline
  final def scaleSelf(aScale:Double):this.type = {
    scale(aScale)
    this
  }
  
  /**
   * same as add, it only returns 'this'
   */
  @inline
  final def addSelf(that:Tuple3d):this.type = {
    add(that)
    this
  }
  
  /**
   * same as sub, it only returns 'this'
   */
  @inline
  final def subSelf(that:Tuple3d):this.type = {
    sub(that)
    this
  }

  /**
   * same as absolute, it only returns 'this'
   */
  @inline
  final def absoluteSelf:this.type = {
    absolute
    this
  }
  
  /**
   * same as negate, it only returns 'this'
   */
  @inline
  final def negateSelf:this.type = {
    negate
    this
  }
  
  /**
   * same as clampMax, it only returns 'this'
   */
  final def clampMaxSelf(clamp:Double):this.type = {
    clampMax(clamp)
    this
  }
  
  /**
   * same as clampMin, it only returns 'this'
   */
  @inline
  final def clampMinSelf(clamp:Double):this.type = {
    clampMin(clamp)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  @inline
  final def setSelf(t:Tuple3d):this.type = {
    set(t)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  final def setSelf(t:Tuple3f):this.type = {
    set(t)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  @inline
  final def setSelf(x:Double,y:Double,z:Double):this.type = {
    set(x,y,z)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  @inline
  def setSelf(x:Float,y:Float,z:Float):this.type = {
    set(x,y,z)
    this
  }
   
  /**
   * Gives the distance between two point in the xy plane (Z is ignored)
   */
  @inline
  final def xyDistanceSqr(that:Tuple3d):Double = {
    val deltaX = this.x-that.x
    val deltaY = this.y-that.y
    deltaX*deltaX+deltaY*deltaY
  }
  
  /**
   * Gives the distance between two point in the xy plane (Z is ignored)
   */
  @inline
  final def xyDistance(that:Tuple3d):Double = math.sqrt(xyDistanceSqr(that))
}