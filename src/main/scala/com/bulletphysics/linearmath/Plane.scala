package com.bulletphysics.linearmath

import javax.vecmath.Tuple3d
import com.bulletphysics.BulletGlobals

object Plane {
  
  object Classifier extends Enumeration {
   type Classifier = Value
   val FRONT, BACK, ON_PLANE = Value
  }
  
  class IntersectionResult {
    
    var hasResult = false
    val prevPoint = new Point3dE
    val nextPoint = new Point3dE
    
    private[Plane] val tmpV = new Vector3dE
    
    private[Plane] val abL = new Line3d
    private[Plane] val acL = new Line3d
    private[Plane] val bcL = new Line3d
    
    private[Plane] val abP = new Point3dE
    private[Plane] var abD = 0d
    private[Plane] var hasAb = false
    private[Plane] val acP = new Point3dE
    private[Plane] var acD = 0d
    private[Plane] var hasAc = false
    private[Plane] val bcP = new Point3dE
    private[Plane] var bcD = 0d
    private[Plane] var hasBc = false
  }
}

class Plane(val origin:Point3dE, val normal:Vector3dE) { 
  
  def this() = this(new Point3dE, new Vector3dE)
  
  /**
   * Classifies the relative position of the given point to the plane using
   * the given tolerance.
   * @param throwAwayPoint a trash variable containing the samplepoint, it will be modified 
   * @return One of the 3 classification types: FRONT, BACK, ON_PLANE
   */
  def classifyPoint(throwAwayPoint:Vector3dE, tolerance:Double):Plane.Classifier.Value ={
    val d = throwAwayPoint.subSelf(origin).normalizeSelf.dot(normal);
    if (d < -tolerance) {
      return Plane.Classifier.FRONT;
    } else if (d > tolerance) {
      return Plane.Classifier.BACK;
    }
    return Plane.Classifier.ON_PLANE;
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
  
  def getZIntersectionWithTriangle(tri:Triangle, currentPos:Line3d, result:Plane.IntersectionResult) = {
    if (currentPos.dir.x == 0 && currentPos.dir.y == 0 && currentPos.dir.z == 0 ) {
      println("no direction at all, wtf?. Debug me!")
    }
    
    // setup the trigometry
    result.abL.origin.set(tri.a)
    result.abL.dir.setSelf(tri.b).sub(tri.a)
    result.acL.origin.set(tri.a)
    result.acL.dir.setSelf(tri.c).sub(tri.a)
    result.bcL.origin.set(tri.b)
    result.bcL.dir.setSelf(tri.c).sub(tri.b)
       
    {
      val distance = getIntersectionWithLine(result.abL,result.tmpV)
      result.hasAb = !(distance.isNaN || distance < 0 || distance > 1)
      if (result.hasAb) {
        result.abL.getPointAtDistance(result.abP, distance)
      }
    }
    {
      val distance = getIntersectionWithLine(result.acL,result.tmpV)
      result.hasAc = !(distance.isNaN || distance < 0 || distance > 1)
      if (result.hasAc) {
        result.acL.getPointAtDistance(result.acP, distance)
      }
    }
    {
      val distance = getIntersectionWithLine(result.bcL,result.tmpV)
      result.hasBc = !(distance.isNaN || distance < 0 || distance > 1)
      if (result.hasBc) {
        result.bcL.getPointAtDistance(result.bcP, distance)
      }
    }
    
    // reduce to two hits at maximum
    if (result.hasAb && result.hasAc && result.hasBc) {
      if (result.abP.equals(result.acP) || result.abP.equals(result.bcP)){
        result.hasAb = false
      } else if (result.acP.equals(result.bcP)){
        result.hasAc = false
      }
    }
    if (result.hasAb) {
      result.abD = result.tmpV.setSelf(result.abP).subSelf(currentPos.origin).xyDot(currentPos.dir).signum*currentPos.origin.xyDistanceSqr(result.abP)
    }
    if (result.hasAc) {
      result.acD = result.tmpV.setSelf(result.acP).subSelf(currentPos.origin).xyDot(currentPos.dir).signum*currentPos.origin.xyDistanceSqr(result.acP)
    }
    if (result.hasBc) {
      result.bcD = result.tmpV.setSelf(result.bcP).subSelf(currentPos.origin).xyDot(currentPos.dir).signum*currentPos.origin.xyDistanceSqr(result.bcP)
    }
    
    if (result.hasAb) {
      if (result.hasAc) {
        if (result.hasBc) {
          System.err.println("All 3 points are present. This should not happend, point reduction does not work. Debug me")
          if (result.abD > result.acD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.acP)
            result.hasResult = true
            //List(result.abP,result.acP)
          } else {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.abP)
            result.hasResult = true
            //List(result.acP,result.abP)
          }
        } else {
          if (result.abD > result.acD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.acP)
            result.hasResult = true
            //List(result.abP,result.acP)
          } else {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.abP)
            result.hasResult = true
            //List(result.acP,result.abP)
          }
        }
      } else {
        if (result.hasBc) {
          List(result.abP,result.bcP)
          if (result.abD > result.bcD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.bcP)
            result.hasResult = true
            //List(result.abP,result.bcP)
          } else {
            result.nextPoint.set(result.bcP)
            result.prevPoint.set(result.abP)
            result.hasResult = true
            //List(result.bcP,result.abP)
          }
        } else {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.abP)
          result.prevPoint.set(result.abP)
          result.hasResult = true
          //List(result.abP,result.abP)
        }
      }
    } else {
      if (result.hasAc) {
        if (result.hasBc) {
          if (result.acD > result.bcD) {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.bcP)
            result.hasResult = true
            //List(result.acP,result.bcP)
          } else {
            result.nextPoint.set(result.bcP)
            result.prevPoint.set(result.acP)
            result.hasResult = true
            //List(result.bcP,result.acP)
          }          
        } else {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.acP)
          result.prevPoint.set(result.acP)
          result.hasResult = true
          //List(result.acP,result.acP)
        }
      } else {
        if (result.hasBc) {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.bcP)
          result.prevPoint.set(result.bcP)
          result.hasResult = true
          //List(result.bcP,result.bcP)
        } else {
          // should not really happend
          System.err.println("A triangle with no plane intersection at all?")
          result.hasResult = false
        }
      }
    }
    result.hasResult
    /*
    val l3 = l1.distinct
    val l4 = l3.map(p => (p,result.tmpV.setSelf(p).subSelf(currentPos.origin).xyDot(currentPos.dir).signum*currentPos.origin.xyDistanceSqr(p)))
    val l5 = l4.sortBy(line => -line._2)
    if (l5.size == 0) {
      println("no intersections at all, wtf?")
      println("  currentPos="+ currentPos)
      println("  tri="+ tri)
      println("  hasAb="+ result.hasAb)
      println("  hasAc="+ result.hasAc)
      println("  result="+ result.hasBc)
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
    l6*/
  }
  
  override
  def toString = {
    "o:" + origin + " n:" + normal
  }
}

