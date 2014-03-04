package com.bulletphysics.linearmath
import javax.vecmath.Tuple3d

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
}