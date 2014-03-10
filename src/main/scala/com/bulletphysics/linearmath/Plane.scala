package com.bulletphysics.linearmath

import javax.vecmath.Tuple3d
import com.bulletphysics.BulletGlobals

object Plane {
  val ONE_P = 1d + BulletGlobals.FLT_EPSILON
  val ONE_M = 1d - BulletGlobals.FLT_EPSILON
  val ZERO_P = BulletGlobals.FLT_EPSILON
  val ZERO_M = -BulletGlobals.FLT_EPSILON
  
  object Classifier extends Enumeration {
   type Classifier = Value
   val FRONT, BACK, ON_PLANE = Value
  }
  
  class IntersectionResult {
    
    var hasResult = false
    val prevPoint = new Point3dE
    var prevPointSqDistance = 0d
    val nextPoint = new Point3dE
    var nextPointSqDistance = 0d
      
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
   * Calculates the intersection point between a plane and a line3d.
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
  
  /**
   * Calculates the intersection points between this plane and the triangle
   * But it does it by assuming this is a plane with a normal perpendicular to (0,0,1)
   * so i guess the whole thing should be renamed and/or moved.
   */
  def getZIntersectionWithTriangle(tri:Triangle, samplePos:Line3d, distanceToPos:Point3dE, result:Plane.IntersectionResult) = {
    if (samplePos.dir.isZeroVector) {
      println("no direction at all?? Debug me!")
    }
    
    // setup the trigometry
    result.abL.origin.set(tri.a)
    result.abL.dir.setSelf(tri.b).sub(tri.a)
    result.acL.origin.set(tri.a)
    result.acL.dir.setSelf(tri.c).sub(tri.a)
    result.bcL.origin.set(tri.b)
    result.bcL.dir.setSelf(tri.c).sub(tri.b)
     
    {
      val distanceAB = getIntersectionWithLine(result.abL,result.tmpV)
      if (distanceAB < Plane.ZERO_P && distanceAB > Plane.ZERO_M) {
        result.hasAb = true
        result.abP.set(tri.a)
      } else 
      if( distanceAB < Plane.ONE_P && distanceAB > Plane.ONE_M) {
        result.hasAb = true
        result.abP.set(tri.b)
      } else {
        result.hasAb = distanceAB >= 0 && distanceAB <= 1
        if (result.hasAb) {
          result.abL.getPointAtDistance(result.abP, distanceAB)
        }  
      }
    }
    {
      val distanceAC = getIntersectionWithLine(result.acL,result.tmpV)
      if (distanceAC < Plane.ZERO_P && distanceAC > Plane.ZERO_M) {
        result.hasAc = true
        result.acP.set(tri.a)
      } else 
      if( distanceAC < Plane.ONE_P && distanceAC > Plane.ONE_M) {
        result.hasAc = true
        result.acP.set(tri.c)
      } else {
        result.hasAc = distanceAC >= 0 && distanceAC <= 1
        if (result.hasAc) {
          result.acL.getPointAtDistance(result.acP, distanceAC)
        }
      }
    }
    {
      val distanceBC = getIntersectionWithLine(result.bcL,result.tmpV)
      if (distanceBC < Plane.ZERO_P && distanceBC > Plane.ZERO_M) {
        result.hasBc = true
        result.bcP.set(tri.b)
      } else 
      if( distanceBC < Plane.ONE_P && distanceBC > Plane.ONE_M) {
        result.hasBc = true
        result.bcP.set(tri.c)
      } else {
        result.hasBc = distanceBC >= 0 && distanceBC <= 1
        if (result.hasBc) {
          result.bcL.getPointAtDistance(result.bcP, distanceBC)
        }
      }
    }
    
    // reduce to two hits, one of the hits must be a duplicate if all three are found
    if (result.hasAb && result.hasAc && result.hasBc) {
      if (result.abP.equals(result.acP) || result.abP.equals(result.bcP)){
        result.hasAb = false
      } else if (result.acP.equals(result.bcP)){
        result.hasAc = false
      }
    }
    
    // calculate the distances from samplePos.origin to the two remaining edge points. 
    // Do it with a sign so that the point behind samplePos.origin, in samplePos.dir direction, have a negative distance
    if (result.hasAb) {
      result.abD = result.tmpV.setSelf(result.abP).subSelf(distanceToPos).xyDot(samplePos.dir).signum*distanceToPos.xyDistanceSqr(result.abP)
    }
    if (result.hasAc) {
      result.acD = result.tmpV.setSelf(result.acP).subSelf(distanceToPos).xyDot(samplePos.dir).signum*distanceToPos.xyDistanceSqr(result.acP)
    }
    if (result.hasBc) {
      result.bcD = result.tmpV.setSelf(result.bcP).subSelf(distanceToPos).xyDot(samplePos.dir).signum*distanceToPos.xyDistanceSqr(result.bcP)
    }
   
    // set both point points to invalid if they all have negative distance to distanceToPos
    /*
    if (result.hasAb && result.abD < -BulletGlobals.CONVEX_DISTANCE_MARGIN_2) {
      if (result.hasAc && result.acD < -BulletGlobals.CONVEX_DISTANCE_MARGIN_2) {
        result.hasAb = false
        result.hasAc = false
      } else if (result.hasBc && result.bcD < -BulletGlobals.CONVEX_DISTANCE_MARGIN_2) {
        result.hasAb = false
        result.hasBc = false
      }
    } else if (result.hasAc && result.acD < -BulletGlobals.CONVEX_DISTANCE_MARGIN_2) {
      if (result.hasBc && result.bcD < -BulletGlobals.CONVEX_DISTANCE_MARGIN_2) {
        result.hasAc = false
        result.hasBc = false
      }
    }
    */
    
    if (result.hasAb) {
      if (result.hasAc) {
        if (result.hasBc) {
          System.err.println("All 3 points are still present. This should not happend, point reduction does not work. Debug me")
          if (result.abD > result.acD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.acP)
            result.nextPointSqDistance = result.abD
            result.prevPointSqDistance = result.acD
            result.hasResult = true
          } else {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.abP)
            result.nextPointSqDistance = result.acD
            result.prevPointSqDistance = result.abD
            result.hasResult = true
          }
        } else {
          if (result.abD > result.acD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.acP)
            result.nextPointSqDistance = result.abD
            result.prevPointSqDistance = result.acD
            result.hasResult = true
          } else {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.abP)
            result.nextPointSqDistance = result.acD
            result.prevPointSqDistance = result.abD
            result.hasResult = true
          }
        }
      } else {
        if (result.hasBc) {
          List(result.abP,result.bcP)
          if (result.abD > result.bcD) {
            result.nextPoint.set(result.abP)
            result.prevPoint.set(result.bcP)
            result.nextPointSqDistance = result.abD
            result.prevPointSqDistance = result.bcD
            result.hasResult = true
          } else {
            result.nextPoint.set(result.bcP)
            result.prevPoint.set(result.abP)
            result.nextPointSqDistance = result.bcD
            result.prevPointSqDistance = result.abD
            result.hasResult = true
          }
        } else {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.abP)
          result.prevPoint.set(result.abP)
          result.nextPointSqDistance = result.abD
          result.prevPointSqDistance = result.abD
          result.hasResult = true
        }
      }
    } else {
      if (result.hasAc) {
        if (result.hasBc) {
          if (result.acD > result.bcD) {
            result.nextPoint.set(result.acP)
            result.prevPoint.set(result.bcP)
            result.nextPointSqDistance = result.acD
            result.prevPointSqDistance = result.bcD
            result.hasResult = true
          } else {
            result.nextPoint.set(result.bcP)
            result.prevPoint.set(result.acP)
            result.nextPointSqDistance = result.bcD
            result.prevPointSqDistance = result.acD
            result.hasResult = true
          }          
        } else {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.acP)
          result.prevPoint.set(result.acP)
          result.nextPointSqDistance = result.acD
          result.prevPointSqDistance = result.acD
          result.hasResult = true
        }
      } else {
        if (result.hasBc) {
          System.err.println("A triangle with only one plane intersection?")
          result.nextPoint.set(result.bcP)
          result.prevPoint.set(result.bcP)
          result.nextPointSqDistance = result.bcD
          result.prevPointSqDistance = result.bcD
          result.hasResult = true
        } else {
          // should not really happen if the triangle collision works as it should
          //System.err.println("A triangle with no plane intersection at all?")
          result.hasResult = false
        }
      }
    }
    result.hasResult
  }
  
  override
  def toString = {
    "o:" + origin + " n:" + normal
  }
}

