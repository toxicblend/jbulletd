package com.bulletphysics.test

import com.bulletphysics.linearmath.Vector3dE
import com.bulletphysics.linearmath.Point3dE
import com.bulletphysics.linearmath.Triangle

import com.bulletphysics.linearmath.Plane
import com.bulletphysics.linearmath.Line3d
import scala.collection.mutable.MutableList
import com.bulletphysics.BulletGlobals

import org.scalatest._

class LinMathTest3 extends FlatSpec with Matchers {
  
  val epsilon = BulletGlobals.CONVEX_DISTANCE_MARGIN
  
  def testTriangleZPlane(t:Triangle, pos:Line3d) = {
    val rv = new Plane.IntersectionResult()
    val p = new Plane
    pos.getZPlane(p)
    p.getZIntersectionWithTriangle(t, pos, pos.origin, rv)
    rv
  }

  "triangle test 1 " should "work" in {
    val pos = new Line3d(new Point3dE(43.83018314314398, 17.09442214205432,605), new Vector3dE(-0.12517998475824044, 1.1289418897228813, 0.0))
    val t = new Triangle(new Point3dE(43.83065545957252, 20.455208403405067,-41.794115953108054),
                         new Point3dE(48.7007779897048, 13.637021214999024,-40.93372388222961),
                         new Point3dE(43.83065545957252, 13.637021214999024,-41.95788079573049) )
    
    t.containsXYPoint(pos.origin) should be (true)
    val r = testTriangleZPlane(t, pos)
    
    r.hasResult should be (true)
    r.nextPoint.x should be ((pos.origin.x) +- epsilon)
    r.nextPoint.y should be ((pos.origin.y) +- epsilon)
    //r.nextPoint.z should be ((pos.origin.z) +- epsilon)
  }
  
  "triangle test 2 " should "work" in {
    val pos = new Line3d(new Point3dE(-1.993441223909736E-4, -37.110517548048755,-44.25145907995137),new Vector3dE(0.030534989805176608, -7.91039462536348, 0) )
    val t = new Triangle(new Point3dE(-5.138866369688371E-4, -34.090249190014575, -44.5811485355647),
                         new Point3dE(-5.133725125541734E-4, -40.90841881633599, -43.816262274010974),
                         new Point3dE(-4.870638892673935, -34.090249190014575, -44.52155559275536) )
    
    t.containsXYPoint(pos.origin) should be (true)
    val r = testTriangleZPlane(t, pos)
    
    r.hasResult should be (true)
    //r.nextPoint.x should be ((pos.origin.x) +- epsilon)
    //r.nextPoint.y should be ((pos.origin.y) +- epsilon)
    //r.nextPoint.z should be ((pos.origin.z) +- epsilon)
  }
  (new Point3dE(-1.993441223909736E-4, -37.110517548048755,-44.25145907995137))
(new Point3dE(0.030534989805176608, -7.91039462536348, 0.0))
(new Point3dE(-5.138866369688371E-4, -34.090249190014575,-44.5811485355647))
(new Point3dE(-5.133725125541734E-4, -40.90841881633599,-43.816262274010974))
(new Point3dE(-4.870638892673935, -34.090249190014575,-44.52155559275536))
}