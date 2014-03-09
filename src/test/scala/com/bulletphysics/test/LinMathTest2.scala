package com.bulletphysics.test

import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.Triangle
import com.bulletphysics.linearmath.Plane
import com.bulletphysics.linearmath.Line3d
import com.bulletphysics.linearmath.Matrix4dE
import scala.collection.mutable.MutableList
import javax.vecmath.AxisAngle4d
import com.bulletphysics.BulletGlobals

import org.scalatest._

class LinMathTest2 extends FlatSpec with Matchers {
  
  val epsilon = 0.00001
  val XVECTOR = new Vector3dE(1,0,0)
  
  def testTriangleZPlane(t:Triangle, pos:Line3d) = {
    val rv = new Plane.IntersectionResult()
    val p = new Plane
    pos.getZPlane(p)
    p.getZIntersectionWithTriangle(t, pos, rv)
    rv
  }
  
  "line to plane 0" should "work" in {
    val pos = new Line3d(new Point3dE(0,0,0), new Vector3dE(.1,.1,0))
    val t = new Triangle(new Point3dE(-1,-1,-1), new Point3dE(1,-1,1), new Point3dE(0,1,1))
    val m = new Matrix4dE
    
    val d = Double.NaN
    d should not equal (0)
    
    (0 until 36000).foreach(angle => {
      m.set(new AxisAngle4d(0,0,1, 0.01*BulletGlobals.SIMD_RADS_PER_DEG*angle))
      m.transform(t)
      val r = testTriangleZPlane(t, pos)
      r.hasResult should be (true)
      val nextPoint = new Vector3dE(r.nextPoint)
      val prevPoint = new Vector3dE(r.prevPoint)
      /*println("angle = " + angle)
      println("pos = " + pos + " pos.angle=" + new Vector3dE(pos.dir).xyAngle(XVECTOR)*BulletGlobals.SIMD_DEGS_PER_RAD)
      println("nextPoint actual: " + nextPoint.xyAngle(XVECTOR)*BulletGlobals.SIMD_DEGS_PER_RAD + " expected 45")
      println("prevPoint actual: " + prevPoint.xyAngle(XVECTOR)*BulletGlobals.SIMD_DEGS_PER_RAD + " expected -135")
      println("nextPoint actual: " + nextPoint)
      println("prevPoint actual: " + prevPoint)
      println("XVECTOR :" + XVECTOR)
      */
      val na = nextPoint.xyAngle(XVECTOR)*BulletGlobals.SIMD_DEGS_PER_RAD
      if (math.abs(na-45d) > epsilon){
        println("debug point")
      }
      na should be ((45d) +- epsilon)
      val pa = prevPoint.xyAngle(XVECTOR)*BulletGlobals.SIMD_DEGS_PER_RAD
      if (math.abs(pa-135) > epsilon){
        println("debug point")
      }
      pa should be ((135d) +- epsilon)
      nextPoint.xyDot(pos.dir) should be > 0d
      prevPoint.xyDot(pos.dir) should be < 0d
    })
  }
  
}