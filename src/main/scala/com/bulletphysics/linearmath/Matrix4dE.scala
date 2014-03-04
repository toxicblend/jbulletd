package com.bulletphysics.linearmath
import javax.vecmath.Matrix4d
import javax.vecmath.Vector3d
import javax.vecmath.Tuple3d

class Matrix4dE(__v00:Double, __v01:Double, __v02:Double, __v03:Double,
                __v10:Double, __v11:Double, __v12:Double, __v13:Double,  
                __v20:Double, __v21:Double, __v22:Double, __v23:Double,
                __v30:Double, __v31:Double, __v32:Double, __v33:Double) 
             extends Matrix4d(__v00, __v01, __v02, __v03,
                              __v10, __v11, __v12, __v13,  
                              __v20, __v21, __v22, __v23,
                              __v30, __v31, __v32, __v33) {
  
  def this() = this(1d,0d,0d,0d, 0d,1d,0d,0d, 0d,0d,1d,0d, 0d,0d,0d,1d) // set unity as default
                              
  def this(v:IndexedSeq[Double]) = this ( v( 0),v( 1),v( 2),v( 3),
                                          v( 4),v( 5),v( 6),v( 7),
                                          v( 8),v( 9),v(10),v(11),
                                          v(12),v(13),v(14),v(15))
                                          
  def this(m:Matrix4d) = this(m.m00, m.m01, m.m02, m.m03,
                              m.m10, m.m11, m.m12, m.m13,
                              m.m20, m.m21, m.m22, m.m23,
                              m.m30, m.m31, m.m32, m.m33)
  
 /**
  * Constructs this matrix as a translate and scale matrix 
  */                                                              
 def this(__trans:Tuple3d, __scale:Tuple3d) = 
    this(__scale.x,         0,         0, __trans.x,
                 0, __scale.y,         0, __trans.y,
                 0,         0, __scale.z, __trans.z,
                 0,         0,         0,         1) 
 
 /**
  * Constructs this matrix as a translate and scale matrix 
  */                                                              
 def this(__trans:Tuple3d, __scale:Double) = 
    this(__scale,       0,       0, __trans.x,
               0, __scale,       0, __trans.y,
               0,       0, __scale, __trans.z,
               0,       0,       0,         1)
  /**
   * converts this matrix into a scale and translate matrix (in place)
   */
  def set(translate:Tuple3d, scale:Tuple3d):Matrix4dE = {
    m00=scale.x; m01=0;       m02=0;       m03=translate.x
    m10=0;       m11=scale.y; m12=0;       m13=translate.y
    m20=0;       m21=0;       m22=scale.z; m23=translate.z
    m30=0;       m31=0;       m32=0;       m33=1
    this
  }
  
  /**
   * converts this matrix into a scale and translate matrix (in place)
   */
  def setThis(translate:Vector3d,scale:Double):Matrix4dE = {
    this.set(scale,translate)
    this
  }
  
  /**
   * converts this matrix into a scale and translate matrix (in place)
   */
  def setThis(scale:Double,translate:Vector3d):Matrix4dE = {
    this.set(scale,translate)
    this
  }
  
  /**
   * transforms the AABB in place
   */
  def transform(aabb:AABB):Matrix4dE = {
    transform(aabb.aabbMax)
    transform(aabb.aabbMin)
    this
  }
  
  /**
   * Inverts this matrix in place and returns this
   */
  def invertSelf = {
    invert
    this
  }
}