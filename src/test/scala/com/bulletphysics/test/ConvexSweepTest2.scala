package com.bulletphysics.test

import javax.vecmath.Vector3d
import javax.vecmath.AxisAngle4d
import scala.collection.mutable.MutableList
import org.scalatest._

class ConvexSweepTest2 extends FlatSpec with Matchers {
  
  val CLOSETO1 = 0.9999
  
  val model = new Model(new Vector3d)
  model.addBox(new Vector3d, new Vector3d(2, 2, 2))
  //model.addTriangle(new Vector3d(0,0,0), new Vector3d(0,1,0), new Vector3d(1,0,0))
  //model.addTriangle(new Vector3d(1,1,1), new Vector3d(1,2,1), new Vector3d(2,1,1))
  
  val models = Array(model)
  val jbcw = new CollisionWrapper(models,5)
  val epsilon = 0.01
  //val convexShape = jbcw.addShereShape(0.5)
  val margin = 0.0001
  val coneHeight = 1d
  val coneZCorrection = -coneHeight*0.5
  val coneRadius = 1d
  val convexShape = jbcw.addConeShapeZ(coneRadius-margin, coneHeight-margin, margin)
  val rot = new AxisAngle4d(1d,0,0,math.Pi) // cone needs to be flipped upside down
  
  "convex test1" should "collide on the tip" in {
    val fromV = new Vector3d(0.1, 0.1, 10)
    val toV = new Vector3d(0.1, 0.1, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    //println("toV:" + toV)
    //println("result:" + result)
    rv should be (true)
    //result.x should be ((toV.x) +- epsilon)
    //result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((1d) +- epsilon)
  }
  
  "convex test2" should "collide on the tip" in {
    val fromV = new Vector3d(CLOSETO1, CLOSETO1, 10)
    val toV = new Vector3d(CLOSETO1, CLOSETO1, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    //result.x should be ((toV.x) +- epsilon)
    //result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((1d) +- epsilon)
  }
  
  "convex test3" should "collide on the tip" in {
    val fromV = new Vector3d(-CLOSETO1, CLOSETO1, 10)
    val toV = new Vector3d(-CLOSETO1, CLOSETO1, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    //result.x should be ((toV.x) +- epsilon)
    //result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((1d) +- epsilon)
  }
  
  "convex test4" should "collide on the tip" in {
    val fromV = new Vector3d(0, -CLOSETO1, 10)
    val toV = new Vector3d(0, -CLOSETO1, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    //result.x should be ((toV.x) +- epsilon)
    //result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((1d) +- epsilon)
  }
  
  "convex test5" should "collide on the tip" in {
    val fromV = new Vector3d(-CLOSETO1, -CLOSETO1, 10)
    val toV = new Vector3d(-CLOSETO1, -CLOSETO1, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    //result.x should be ((toV.x) +- epsilon)
    //result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((1d) +- epsilon)
  }
  
  "convex test6" should "collide on the side" in {
    val fromV = new Vector3d(-5d, 0, 0)
    val toV = new Vector3d(5d, 0, 0)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((-1d-coneRadius) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z should be ((toV.z) +- epsilon)
  }
  
  "convex test7" should "collide on the side" in {
    val fromV = new Vector3d(5d, 0, 0)
    val toV = new Vector3d(-5d, 0, 0)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((1d+coneRadius) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z should be ((toV.z) +- epsilon)
  }
  
  "convex test8" should "collide on the lip of the cone" in {
    val fromV = new Vector3d(1d+CLOSETO1, 0, 10)
    val toV = new Vector3d(1d+CLOSETO1, 0, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((toV.x) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((0d) +- epsilon)
  }
  
  "convex test9" should "miss" in {
    val fromV = new Vector3d(2.01d, 0, 10)
    val toV = new Vector3d(2.01d, 0, -10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (false)
  }
  
  "convex test10" should "collide on the flat side" in {
    val fromV = new Vector3d(0, 0, -10)
    val toV = new Vector3d(0, 0, 10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((toV.x) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z-coneZCorrection should be ((-1d) +- epsilon)
  }
  
  "convex test11" should "collide on the flat side" in {
    val fromV = new Vector3d(1.5d, 0, -10)
    val toV = new Vector3d(1.5d, 0, 10)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((toV.x) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z-coneZCorrection should be ((-1d) +- epsilon)
  }
  
  "convex test12" should "collide just fine" in {
    val fromV = new Vector3d(10, -CLOSETO1, -coneZCorrection)
    val toV = new Vector3d(-10, -CLOSETO1, -coneZCorrection)
    val result = new Vector3d()
    val rv = jbcw.convexSweepTest(convexShape,fromV,toV,rot,result)
    rv should be (true)
    result.x should be ((1d+coneRadius) +- epsilon)
    result.y should be ((toV.y) +- epsilon)
    result.z+coneZCorrection should be ((0d) +- epsilon)
  }
  
  "convex test cleanup" should "not fail" in {
    jbcw.destroy
  }
}