package com.bulletphysics.test

import javax.vecmath.Vector3d
import javax.vecmath.AxisAngle4d
import scala.collection.mutable.MutableList
import org.scalatest._

class ConvexSweepTest1 extends FlatSpec with Matchers {
  
  val CLOSETO1 = 0.9999
  
  val model = new Model(new Vector3d)
  model.addBox(new Vector3d, new Vector3d(2, 2, 2))
  //model.addTriangle(new Vector3d(0,0,0), new Vector3d(0,1,0), new Vector3d(1,0,0))
  //model.addTriangle(new Vector3d(1,1,1), new Vector3d(1,2,1), new Vector3d(2,1,1))
  
  val models = Array(model)
  val facade = new BulletFacade(models,5)
  val epsilon = 0.01
  val rot = new AxisAngle4d(1d,0,0,math.Pi)
  val margin = 0.0001
  val coneHeight = 1d
  val coneZCorrection = -coneHeight*0.5
  val coneRadius = 1d
  val convexShape = facade.addConeShapeZ(coneRadius-margin,coneHeight-margin,margin)
  //val convexShape = jbcw.addConeShapeZ(1, 1, margin)  
      
  "convex test0" should "collide on the tip" in {
    (-20 to 20 ).map(x => {
      val fromV = new Vector3d(x*0.1, 0, 30)
      val toV = new Vector3d(x*0.1, 0, -30)
      val result = new Vector3d()
      //val convexShape = jbcw.addShereShape(0.001)
      
      //println("fromV:" + fromV + " toV " + toV)
      val rv = facade.convexSweepTest(convexShape,fromV,toV,rot,result)
      //println("toV:" + toV)
      //println("result:" + result.x)
      rv should be (true)
      result.x should be ((fromV.x) +- epsilon)
      result.y should be ((toV.y) +- epsilon)
      (x*0.1, result)
      //result.y should be ((toV.y) +- epsilon)
      //result.z should be ((1d) +- epsilon)
    })
  }
  
  "convex test cleanup" should "not fail" in {
    facade.destroy
  }
}