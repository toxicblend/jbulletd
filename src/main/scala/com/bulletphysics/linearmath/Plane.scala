package com.bulletphysics.linearmath

import javax.vecmath.Tuple3d
import com.bulletphysics.BulletGlobals

object Classifier extends Enumeration {
  type Classifier = Value
  val FRONT, BACK, ON_PLANE = Value
}

class Plane(val origin:Point3dE, val normal:Vector3dE) { 
  
  def this() = this(new Point3dE, new Vector3dE)
  
  
  /**
   * Classifies the relative position of the given point to the plane using
   * the given tolerance.
   * @param throwAwayPoint a trash variable containing the samplepoint, it will be modified 
   * @return One of the 3 classification types: FRONT, BACK, ON_PLANE
   */
  def classifyPoint(throwAwayPoint:Vector3dE, tolerance:Double):Classifier.Value ={
    val d = throwAwayPoint.subSelf(origin).normalizeSelf.dot(normal);
    if (d < -tolerance) {
      return Classifier.FRONT;
    } else if (d > tolerance) {
      return Classifier.BACK;
    }
    return Classifier.ON_PLANE;
  }
  
  def setSelf(t:Triangle):Plane = {
    t.computeCentroid(origin)
    t.computeNormal(normal)
    this
  }
  
  /**
   * Calculates the intersection point between plane and line3d.
   * 
   * @param line
   * @param tmp intermediate storage
   * @return the distance between the intersection point and ray.origin
   */
  def getIntersectionWithLine(line:Line3d, tmp:Vector3dE):Double = {
    val denom = normal.dot(line.dir)
    if (math.abs(denom) > BulletGlobals.FLT_EPSILON) {
      normal.dot(tmp.setSelf(origin).subSelf(line.origin)) / denom;
    } else {
      Double.NaN
    }
  }
  
  def getZIntersectionWithTriangle(tri:Triangle, currentPos:Line3d) = {
    if (currentPos.dir.x == 0 && currentPos.dir.y == 0 && currentPos.dir.z == 0 ) {
      println("no direction at all, wtf?. Debug me!")
    }
    
    val rab = new Line3d(tri.a, new Vector3dE(tri.b).subSelf(tri.a))
    val rac = new Line3d(tri.a, new Vector3dE(tri.c).subSelf(tri.a))
    val rbc = new Line3d(tri.b, new Vector3dE(tri.c).subSelf(tri.b))
    
    val tmp:Vector3dE = new Vector3dE
    val labi = getIntersectionWithLine(rab,tmp)
    val laci = getIntersectionWithLine(rac,tmp)
    val lbci = getIntersectionWithLine(rbc,tmp)
    val l1 = List( (rab,labi), (rac,laci), (rbc,lbci))
    val l2 = l1.filter( ray => (!ray._2.isNaN && (ray._2 >= 0d) && (ray._2 <= 1d) ))
    val l3 = l2.map(ray => ray._1.getPointAtDistance(new Point3dE, ray._2)).distinct
    val l4 = l3.map(p => (p,tmp.setSelf(p).subSelf(currentPos.origin).xyDot(currentPos.dir).signum*currentPos.origin.xyDistanceSqr(p)))
    val l5 = l4.sortBy(ray => -ray._2)
    if (l5.size == 0) {
      println("no intersections at all, wtf?")
      println("  currentPos="+ currentPos)
      println("  tri="+ tri)
      println("  rabi="+ labi)
      println("  raci="+ laci)
      println("  rbci="+ lbci)
      println("")
    }
    val l6 = (l5.head,l5.last)
    if (l6._1._2 < 0) {
      println("Closest forward intersection is behind us, wtf?")
      println("  currentPos="+ currentPos)
      println("  intersection="+ l6._1._1)
      val v = new Vector3dE(l6._1._1).subSelf(currentPos.origin)
      println("  dir.dot(v_to_intersection)=" +  currentPos.dir.xyDot(v) )
      println("")
    }
    l6
  }
  
  override
  def toString = {
    "o:" + origin + " n:" + normal
  }
}

