package com.bulletphysics.test

import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.Triangle

import com.bulletphysics.linearmath.Plane
import com.bulletphysics.linearmath.Line3d
import scala.collection.mutable.MutableList
import org.scalatest._

class LinMathTest1 extends FlatSpec with Matchers {
  
  val epsilon = 0.00001
  
  def testTriangleZPlane(t:Triangle, pos:Line3d) = {
    val rv = new Plane.IntersectionResult()
    val p = new Plane
    pos.getZPlane(p)
    p.getZIntersectionWithTriangle(t, pos, rv)
    rv
  }
  
  "ray to plane 0" should "work" in {
    val r100 = new Line3d(new Vector3dE(1,0,0))
    val r010 = new Line3d(new Vector3dE(0,1,0))
    val r001 = new Line3d(new Vector3dE(0,0,1))

    val plane = new Plane
    r100.getZPlane(plane)
    plane.origin.x should be (0) 
    plane.origin.y should be (0) 
    plane.origin.z should be (0)
    
    plane.normal.x should be (0) 
    math.abs(plane.normal.y) should be (1) 
    plane.normal.z should be (0)
    
    r010.getZPlane(plane)
    plane.origin.x should be (0) 
    plane.origin.y should be (0) 
    plane.origin.z should be (0)
    
    math.abs(plane.normal.x) should be (1) 
    plane.normal.y should be (0) 
    plane.normal.z should be (0)
  }

  "triangle test 1 " should "work" in {
    val pos = new Line3d(new Vector3dE(1,1,0))
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)

    //r.size should be (2)
    r.hasResult should be (true)
    r.nextPoint.x should be (0.5)
    r.nextPoint.y should be (0.5)
    r.nextPoint.z should be (1)
  }
  
   "TrianglePlaneIntersectionTest 1" should "find intersections" in {  
    val pos = new Line3d(new Point3dE(0,1,1), new Vector3dE(-1f,-1f,0))
    val t = new Triangle(new Point3dE(0f,1f,0f), new Point3dE(1f,2f,0), new Point3dE(0f,1f,1f))
    val r = testTriangleZPlane(t, pos)
    r.hasResult should be (false)
  }
   
  "TrianglePlaneIntersectionTest 2" should "find intersections crossing all 3 lines of triangle (origo+hyp)" in {  
    val pos = new Line3d(new Point3dE(.2f,.2f,1f), new Vector3dE(1f,1f,0))
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)
    
    r.hasResult should be (true)
    
    r.prevPoint.x should be ((0d) +- epsilon)
    r.prevPoint.y should be ((0d) +- epsilon)
    r.prevPoint.z should be ((1d) +- epsilon)
    
    r.nextPoint.x should be ((.5d) +- epsilon)
    r.nextPoint.y should be ((.5d) +- epsilon)
    r.nextPoint.z should be ((1d) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 3" should "find intersections z-hypotenuse plane" in {  
    val pos = new Line3d(new Point3dE(.5f,.5f,1f), new Vector3dE(-1f,1f,0))
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)
    
    r.hasResult should be (true)
    
    r.prevPoint.x should be ((1d) +- epsilon)
    r.prevPoint.y should be ((0d) +- epsilon)
    r.prevPoint.z should be ((1d) +- epsilon)
    
    r.nextPoint.x should be ((0d) +- epsilon)
    r.nextPoint.y should be ((1d) +- epsilon)
    r.nextPoint.z should be ((1d) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 4" should "find intersections in xz plane" in {
    val pos = new Line3d(new Point3dE(.5f,0f,1f), new Vector3dE(1f,0f,0))    
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)
    
    r.hasResult should be (true)
    r.prevPoint.x should be ((0d) +- epsilon)
    r.prevPoint.y should be ((0d) +- epsilon)
    r.prevPoint.z should be ((1d) +- epsilon)
    
    r.nextPoint.x should be ((1d) +- epsilon)
    r.nextPoint.y should be ((0d) +- epsilon)
    r.nextPoint.z should be ((1d) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 5" should "find intersections in the yz plane" in { 
    val pos = new Line3d(new Point3dE(0f,.5f,1f), new Vector3dE(0f,1f,0))    
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)

    r.hasResult should be (true)
    
    r.prevPoint.x should be ((0d) +- epsilon)
    r.prevPoint.y should be ((0d) +- epsilon)
    r.prevPoint.z should be ((1d) +- epsilon)
    
    r.nextPoint.x should be ((0d) +- epsilon)
    r.nextPoint.y should be ((1d) +- epsilon)
    r.nextPoint.z should be ((1d) +- epsilon)
  }
  
  "TrianglePlaneIntersectionTest 6" should "find nothing" in { 
    val pos = new Line3d(new Point3dE(1.1f,1.1,1f), new Vector3dE(0f,1f,0))    
    val t = new Triangle(new Point3dE(0,0,1), new Point3dE(1,0,1), new Point3dE(0,1,1))
    val r = testTriangleZPlane(t, pos)

    r.hasResult should be (false)
  }
}