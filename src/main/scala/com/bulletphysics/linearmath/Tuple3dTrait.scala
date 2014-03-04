package com.bulletphysics.linearmath
import javax.vecmath.Tuple3d
import javax.vecmath.Tuple3f

trait Tuple3dTrait extends Tuple3d {
   
  /**
   * same as scale, it only returns 'this'
   */
  def scaleSelf(aScale:Double):this.type = {
    scale(aScale)
    this
  }
  
  /**
   * same as add, it only returns 'this'
   */
  def addSelf(that:Tuple3d):this.type = {
    add(that)
    this
  }
  
  /**
   * same as sub, it only returns 'this'
   */
  def subSelf(that:Tuple3d):this.type = {
    sub(that)
    this
  }

  /**
   * same as absolute, it only returns 'this'
   */
  def absoluteSelf:this.type = {
    absolute
    this
  }
  
  /**
   * same as negate, it only returns 'this'
   */
  def negateSelf:this.type = {
    negate
    this
  }
  
  /**
   * same as clampMax, it only returns 'this'
   */
  def clampMaxSelf(clamp:Double):this.type = {
    clampMax(clamp)
    this
  }
  
  /**
   * same as clampMin, it only returns 'this'
   */
  def clampMinSelf(clamp:Double):this.type = {
    clampMin(clamp)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  def setSelf(t:Tuple3d):this.type = {
    set(t)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  def setSelf(t:Tuple3f):this.type = {
    set(t)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  def setSelf(x:Double,y:Double,z:Double):this.type = {
    set(x,y,z)
    this
  }
  
  /**
   * same as set, it only returns 'this'
   */
  def setSelf(x:Float,y:Float,z:Float):this.type = {
    set(x,y,z)
    this
  }
  
  /**
   * Gives the distance between two point in the xy plane (Z is ignored)
   */
  @inline
  def xyDistance(that:Tuple3d):Double = {
    val deltaX = this.x-that.x
    val deltaY = this.y-that.y
    math.sqrt(deltaX*deltaX+deltaY*deltaY)
  }
}